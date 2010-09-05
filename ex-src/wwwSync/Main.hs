{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Main where

import Control.Applicative
import Data.List
import System.Process
import System.Exit
import System.Environment
import System.IO
import Text.Printf

--------------------------------------------------------------------------------
-- RSYNC Configuration

www, lagrange :: RsyncPath
www      = Local  "/home/nils/www/"
lagrange = Remote "lagrange" "htdocs/n-sch.de/"

-- | Upload all files to the server
rsyncUp :: CreateProcess
rsyncUp = rsync www lagrange $
                [ "-av"
                , "--delete"
                , "--copy-links"
                ]

-- | Download all files from the server
rsyncDown :: CreateProcess
rsyncDown = rsync lagrange www $
                  [ "-av"
                  , "--delete"
                  ] ++ excludelist

  where
    excludelist = map (printf "--exclude=\"%s\"")
                      [ "/hdocs"
                      ]


--------------------------------------------------------------------------------
-- IO functions

runRsync :: CreateProcess -> IO ()
runRsync p = do
    (_,Just hout, Just herr, ph) <- createProcess p { std_out = CreatePipe
                                                    , std_err = CreatePipe }
    exitCode <- waitForProcess ph
    case exitCode of
         ExitSuccess   -> do
             outLog <- mkLog <$> hGetContents hout
             printf "rsync finished:\n\n%s\n" outLog

         ExitFailure _ -> do
             errLog <- mkErrLog <$> hGetContents herr
             outLog <- mkLog    <$> hGetContents hout
             printf "rsync error:\n\n%s\n"  errLog
             printf "Complete log:\n\n%s\n" outLog
             exitFailure
  where
    mkLog    = unlines . map ("> " ++) . lines
    mkErrLog = unlines . map ("! " ++) . lines


main :: IO ()
main = do
    args <- getArgs
    case args of
         ["up"]   -> runRsync rsyncUp
         ["down"] -> runRsync rsyncDown
         _        -> do
             hPutStrLn stderr "up/down expected."
             exitFailure


--------------------------------------------------------------------------------
-- RSYNC Command

type Host = String
type Argument = String

data RsyncPath
    = Local FilePath
    | Remote Host FilePath

rsync :: RsyncPath -> RsyncPath -> [Argument] -> CreateProcess
rsync p1 p2 args = shell $ printf "rsync %s \"%s\" \"%s\"" (genArgs args) (genPath p1) (genPath p2)

  where
    genArgs a = intercalate " " a
    genPath (Local fp)    = fp
    genPath (Remote h fp) = h ++ ":" ++ foldr escapeSpaces "" fp
    escapeSpaces ' ' s = "\\ " ++ s
    escapeSpaces c   s   = c : s
