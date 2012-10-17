module Cain.Utils (

    c, t,
    getDateStr,
    getRecursiveContents,
    copyToFolder

  ) where

import Cain.Core

import Data.String.Utils
import Control.Monad
import System.Directory
import System.FilePath

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


getDateStr :: Date -> String
getDateStr d = (show . year $ d) ++ "-" ++ (show . month $ d) ++ "-" ++ (show . day $ d) 


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