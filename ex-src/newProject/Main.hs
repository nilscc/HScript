module Main where

import System.Directory
import System.Environment
import System.Process
import System.Exit
import Text.Printf

main :: IO ()
main = do
    [dir] <- getArgs
    createDirectoryIfMissing True dir
    ExitSuccess <- system $ printf "tar xzf ~/dev/new-project.tar.gz -C \"%s\"" dir
    printf "New project created.\n"
