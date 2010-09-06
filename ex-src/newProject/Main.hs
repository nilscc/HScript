{-# OPTIONS -fno-warn-unused-do-bind #-}

module Main where

import Control.Applicative
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf


--------------------------------------------------------------------------------
-- Commands

newProjectTar :: FilePath
newProjectTar = "~/dev/new-project.tar.gz"

tarCmd :: FilePath -> CreateProcess
tarCmd     dir = (quiteShell $ printf "tar xzf %s" newProjectTar) { cwd = Just dir }

gitInitCmd :: FilePath -> CreateProcess
gitInitCmd dir = (quiteShell "git init") { cwd = Just dir }


--------------------------------------------------------------------------------
-- IO stuff

runCmd :: CreateProcess -> IO ()
runCmd p = do
    (_, _, Just herr, ph) <- createProcess p { std_err = CreatePipe }
    ex <- waitForProcess ph
    case ex of
         ExitSuccess   -> return ()
         ExitFailure _ -> do
             errLog <- (unlines . map ("! " ++) . lines) <$> hGetContents herr
             printf "Shell error:\n\n%s\n" errLog
             exitFailure


main :: IO ()
main = do
    [dir] <- getArgs
    exists <- doesDirectoryExist dir
    if not exists then do
        createDirectory dir
        runCmd $ tarCmd dir
        runCmd $ gitInitCmd dir
        printf "New project created.\n"
      else do
        printf "Directory already exists.\n"
        exitFailure


--------------------------------------------------------------------------------
-- Helper

quiteShell :: String -> CreateProcess
quiteShell s = (shell s) { std_out = CreatePipe, std_err = CreatePipe }
