module Cain.Build (build) where

import Cain.Core
import Cain.Utils

import Data.Yaml
import Data.String.Utils
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Directory
import System.FilePath ((</>))
import Text.Hastache
import Text.Hastache.Context
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Shared (defaultWriterOptions)
import Text.RSS
import qualified Network.URI as N


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


copyStatic :: IO ()
copyStatic = do
    folders <- filterM doesDirectoryExist [ c "cssFolder"
                                          , c "jsFolder"
                                          , c "imgFolder"
                                          , c "pagesFolder" ]
    staticFiles <- concat <$> mapM getRecursiveContents folders
    mapM_ (copyToFolder (c "buildFolder")) staticFiles
    return ()


validatePostPath :: FilePath -> Bool
validatePostPath path =
    length splitPath == 4  && extension == "md"
  where
    splitPath = split "/" path
    extension = last . split "." . last $ splitPath


getPostContent :: Post -> IO (String, String)
getPostContent post = do 
    let context = mkGenericContext post
    templatedContent <- decodeStrLBS <$> hastacheFile defaultConfig (c "postTemplate") context
    return (filename post, templatedContent)


findPosts :: FilePath -> IO [Post]
findPosts path = do
    files <- getRecursiveContents path
    let validFiles = filter validatePostPath files
    contents <- mapM readFile validFiles
    return $ zipWith (curry buildPost) validFiles contents


buildIndex :: [Post] -> IO String
buildIndex postsL = decodeStrLBS <$> hastacheFile defaultConfig (c "indexTemplate") context
  where
    context = mkGenericContext $ PostCollection postsL


buildPost :: (FilePath, String) -> Post
buildPost (path, fileContent) = 
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


build' :: Config -> [Post] -> IO()
build' config postsL = do
    index        <- buildIndex (reverse . sort $ postsL)
    postContents <- mapM getPostContent postsL
    oldFiles     <- getRecursiveContents $ c "buildFolder"
    
    mapM_ removeFile oldFiles

    copyStatic
    writeIndex index
    mapM_ writePost postContents

    --either putStrLn
    --       (\rss -> putStrLn . showXML . rssToXML $ rss)
    --       (buildRSS config postsL)
    
    print config


build :: IO ()
build = do
    config      <- readConfig
    postsL      <- readPosts

    case build' <$> config <*> postsL of
        Left err -> putStrLn err
        Right b  -> do b; putStrLn "Generated!"