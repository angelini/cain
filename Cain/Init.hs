module Cain.Init (initFolder) where

import Cain.Utils

import Control.Monad
import Control.Applicative
import System.Directory

defaultConfig :: String
defaultConfig = unlines [ "# Basic Cain Configuration"
                        , "main-url:   \"http://your-url.com\""
                        , ""
                        , "rss-title:  \"Sample Title\""
                        , "rss-desc:   \"Sample Description\""
                        , "rss-author: \"Author\""
                        , ""
                        , "s3-bucket:  \"bucket.name\""
                        , "s3-key:     \"S3-KEY\""
                        ]


defaultIndex :: String
defaultIndex = unlines [ "<!doctype html>"
                       , "<html lang=\"en\">"
                       , "<head>"
                       , "  <meta charset=\"utf-8\">"
                       , "  <title>Cain</title>"
                       , "</head>"
                       , "<body>"
                       , "  <div>"
                       , "    <header>"
                       , "      <h1>Cain Sample</h1>"
                       , "    </header>"
                       , "    {{#posts}}"
                       , "      <article>"
                       , "        <div>"
                       , "          <a href=\"/{{filename}}\">{{title}}</a>"
                       , "        </div>"
                       , "        <div>"
                       , "          {{#date}}"
                       , "            <time class=\"timeago\" datetime=\"{{year}}-{{month}}-{{day}}\">{{year}}-{{month}}-{{day}}</time>"
                       , "          {{/date}}"
                       , "        </div>"
                       , "      </article>"
                       , "    {{/posts}}"
                       , "  </div>"
                       , "</body>"
                       , "</html>" ]


defaultPost :: String
defaultPost = unlines [ "<!doctype html>"
                      , "<html lang=\"en\">"
                      , "<head>"
                      , "  <meta charset=\"utf-8\">"
                      , "  <title>Cain</title>"
                      , "</head>"
                      , "<body>"
                      , "  <article class=\"post container\">"
                      , "    <header>"
                      , "      <h1 class=\"title\">{{title}}</h1>"
                      , "      <div class=\"date\">"
                      , "        {{#date}}"
                      , "          <time class=\"timeago\" datetime=\"{{year}}-{{month}}-{{day}}\">{{year}}-{{month}}-{{day}}</time>"
                      , "        {{/date}}"
                      , "      </div>"
                      , "    </header>"
                      , "    <div>"
                      , "      <div>{{{content}}}</div>"
                      , "    </div>"
                      , "  </article>"
                      , "</body>"
                      , "</html>" ]


initFolder :: IO ()
initFolder = do
    mapM_ (createDirectoryIfMissing True) folders
    mapM_ (uncurry writeFile) files
  where
    folders = [ c "postsFolder"
              , c "buildFolder"
              , c "cssFolder"
              , c "jsFolder"
              , c "imgFolder"
              , c "pagesFolder"
              , c "templates" ]
    files = [ (c "indexTemplate", defaultIndex)
            , (c "postTemplate", defaultPost) 
            , (c "confFile", defaultConfig) ]