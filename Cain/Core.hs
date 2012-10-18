{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Cain.Core (

    Date(..),
    Post(..),
    PostCollection(..),
    Config(..)

  ) where

import Data.Data
import Data.Yaml
import Control.Applicative
import Control.Monad


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