{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Cain where

import           Data.Yaml
import           Data.String.Utils
import qualified Data.ByteString as B
import           Data.Data
import           Data.List
import           Data.Maybe
import           Control.Applicative
import           Control.Monad
import           System.Environment (getArgs)
import           System.Directory
import           System.FilePath ((</>))
import           Text.Hastache
import           Text.Hastache.Context
import           Text.Pandoc.Parsing
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML
import           Text.Pandoc.Shared (defaultWriterOptions)
import           Text.RSS
import qualified Network.URI as N

c :: String -> String
c "postsFolder"   = "posts"
c "buildFolder"   = "build"
c "confFile"      = "config.yaml"
c "indexTemplate" = "templates/index.html"
c "postTemplate"  = "templates/post.html"
c "cssFolder"     = "static/css"
c "jsFolder"      = "static/js"
c "imgFolder"     = "static/img"
c "pagesFolder"   = "static/pages"
c _               = error "Unknown Constant"

t :: String -> String
t "post"           = "{{post}}"
t "title"          = "{{title}}"
t "startPostBlock" = "{{startPostBlock}}"
t "endPostBlock"   = "{{endPostBlock}}"
t _                = error "Unknow Tag"


data Date = Date { year  :: Integer
                 , month :: Integer
                 , day   :: Integer
                 } deriving (Show, Eq, Ord, Data, Typeable)


data Post = Post { title    :: String
                 , content  :: String
                 , date     :: Date
                 , filename :: String
                 } deriving (Show, Eq, Data, Typeable)

instance Ord Post where
    (Post _ _ d1 _) `compare` (Post _ _ d2 _) = d1 `compare` d2


data PostCollection = PostCollection { posts :: [Post] 
                                     } deriving (Show, Data, Typeable)


data Config = Config { mainUrl   :: String
                     , rssTitle  :: String
                     , rssDesc   :: String
                     , rssAuthor :: String
                     , s3Bucket  :: String
                     , s3Key     :: String
                     } deriving (Show)

instance FromJSON Config where
    parseJSON (Object v) = 
        Config <$> v .: "main-url"
               <*> v .: "rss-title"
               <*> v .: "rss-desc"
               <*> v .: "rss-author"
               <*> v .: "s3-bucket"
               <*> v .: "s3-key"

    parseJSON _          = mzero


getDateStr :: Date -> String
getDateStr d = (show . year $ d) ++ "-" ++ (show . month $ d) ++ "-" ++ (show . day $ d) 


createPost :: (FilePath, String) -> Post
createPost (path, fileContent) = 
    Post { title    = titleStrSpaces
         , content  = htmlContent
         , date     = dateObj
         , filename = file }
  where
    document = readMarkdown defaultParserState fileContent
    htmlContent = writeHtmlString defaultWriterOptions document

    splitPath = split "/" path
    titleStr = case splitPath of
        (_:_:_:ts:[]) -> last . split "-" . head . split "." $ ts
        _             -> path
    dateObj = case splitPath of
        (_:y:m:ts:[]) -> Date (read y) (read m) (read . head . split "-" $ ts)
        _             -> Date 1970 01 01

    file = getDateStr dateObj ++ "_" ++ replace " " "_" titleStr ++ ".html"
    titleStrSpaces = replace "_" " " titleStr


validatePostPath :: FilePath -> Bool
validatePostPath path =
    length splitPath == 4  && extension == "md"
  where
    splitPath = split "/" path
    extension = last . split "." . last $ splitPath


getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)


copyToFolder :: FilePath -> FilePath -> IO ()
copyToFolder folder file = copyFile file newFile
  where
    name = last $ split "/" file
    newFile = folder </> name


copyStatic :: IO ()
copyStatic = do
    folders <- filterM doesDirectoryExist [ c "cssFolder"
                                          , c "jsFolder"
                                          , c "imgFolder"
                                          , c "pagesFolder" ]
    staticFiles <- concat <$> mapM getRecursiveContents folders
    mapM_ (copyToFolder (c "buildFolder")) staticFiles
    return ()


findPosts :: FilePath -> IO [Post]
findPosts path = do
    files <- getRecursiveContents path
    let validFiles = filter validatePostPath files
    contents <- mapM readFile validFiles
    return $ zipWith (curry createPost) validFiles contents


parseConfig :: B.ByteString -> Either String Config
parseConfig confFile = case decode confFile of
    Just config -> Right config
    Nothing     -> Left $ "Could not parse " ++ c "confFile"


buildPostRSS :: Config -> Post -> Item
buildPostRSS config post = [ Title         (title post)
                           , Link          (fromJust . N.parseURI $ "http://www.google.com")
                           , Description   (content post)
                           , Author        (rssAuthor config)
                           , Category      Nothing ""
                           , Enclosure     (fromJust . N.parseURI $ "http://www.google.com") 7333 "text/html"
                           , Guid          True ""
                           , PubDate       undefined
                           , Source        (fromJust . N.parseURI $ "http://www.google.com") "Description"
                           ]


buildRSS :: Config -> [Post] -> Either String RSS
buildRSS config postsL = if isNothing url
                            then Left "Error Parsing URI"
                            else Right $ RSS (rssTitle config)
                                             (fromJust url)
                                             (rssDesc config)
                                             []
                                             postItems
  where
    url = N.parseURI $ mainUrl config
    postItems = map (buildPostRSS config) postsL


buildIndex :: [Post] -> IO String
buildIndex postsL = decodeStrLBS <$> hastacheFile defaultConfig (c "indexTemplate") context
  where
    context = mkGenericContext $ PostCollection postsL


buildPost :: Post -> IO (String, String)
buildPost post = do 
    let context = mkGenericContext post
    templatedContent <- decodeStrLBS <$> hastacheFile defaultConfig (c "postTemplate") context
    return (filename post, templatedContent)


writeIndex :: String -> IO ()
writeIndex ind = do
    let fileLoc = c "buildFolder" </> "index.html"
    writeFile fileLoc ind
    putStrLn $ "Wrote " ++ fileLoc


writePost :: (String, String) -> IO ()
writePost (file, fileCont) = do
    let fileLoc = c "buildFolder" </> file
    writeFile fileLoc fileCont
    putStrLn $ "Wrote " ++ fileLoc


readConfig :: IO (Either String Config)
readConfig = do
    fileExists <- doesFileExist (c "confFile")
    if fileExists
        then do
            confFile <- B.readFile (c "confFile")
            return $ parseConfig confFile
        else return . Left $ "* " ++ c "confFile" ++ " cannot be found"


readPosts :: IO (Either String [Post])
readPosts = do
    fileExists <- doesDirectoryExist (c "postsFolder")
    if fileExists
        then do
            postsL <- findPosts (c "postsFolder")
            return $ Right postsL
        else return . Left $ "* " ++ c "postsFolder" ++ " cannot be found"


generate' :: Config -> [Post] -> IO()
generate' config postsL = do
    index        <- buildIndex (reverse . sort $ postsL)
    postContents <- mapM buildPost postsL
    oldFiles     <- getRecursiveContents $ c "buildFolder"
    
    mapM_ removeFile oldFiles

    copyStatic
    writeIndex index
    mapM_ writePost postContents

    either putStrLn
           (\rss -> putStrLn . showXML . rssToXML $ rss)
           (buildRSS config postsL)
    
    print config


generate :: IO ()
generate = do
    config      <- readConfig
    postsL      <- readPosts

    case generate' <$> config <*> postsL of
        Left err -> putStrLn err
        Right g  -> do g; putStrLn "Generated!"


printUsage :: IO ()
printUsage = do
    let usage = unlines [ ""
                        , "Usage:"
                        , ""
                        , "cain init      -- Init Cain folder"
                        , "cain generate  -- Build static files"
                        , "cain upload    -- Upload files in /build to S3"
                        ]
    putStrLn usage


main :: IO ()
main = do
    args <- getArgs

    case args of ["init"]     -> undefined
                 ["generate"] -> generate
                 ["upload"]   -> undefined
                 _            -> printUsage
