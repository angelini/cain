module Main (main) where

import Cain

import System.Environment

usage :: String
usage = unlines [ ""
                , "Usage:"
                , ""
                , "cain init      -- Init Cain folder"
                , "cain build     -- Build static files"
                , "cain upload    -- Upload files in /build to S3"
                ]

main :: IO ()
main = do
    args <- getArgs

    case args of ["init"]     -> undefined
                 ["build"]    -> build
                 ["upload"]   -> undefined
                 _            -> putStrLn usage