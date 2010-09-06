{-# OPTIONS -fno-warn-unused-do-bind #-}

module Main where

import Data.List
import System.Environment
import System.Exit
import System.Shell
import Text.Printf

--------------------------------------------------------------------------------
-- RSYNC Configuration

www, lagrange :: RsyncPath
www      = Local  "/home/nils/www/"
lagrange = Remote "lagrange" "htdocs/n-sch.de/"

-- | Upload all files to the server
rsyncUp :: Shell String
rsyncUp = rsync www lagrange $
                [ "-av"
                , "--delete"
                , "--copy-links"
                ]

-- | Download all files from the server
rsyncDown :: Shell String
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

main :: IO ()
main = do
    args <- getArgs
    case args of
         ["up"]   -> runRsync rsyncUp
         ["down"] -> runRsync rsyncDown
         _        -> do printf "up/down expected."
                        exitFailure

runRsync :: Shell String -> IO ()
runRsync sh = do
    res <- shell sh
    case res of
         Right l ->
             printf "rsync finished:\n\n%s\n" (mkLog l)
         Left err -> do
             printf "rsync error:\n\n%s\n"    (mkErrLog err)
             exitFailure
  where
    mkLog    = unlines . map ("> " ++) . lines
    mkErrLog = unlines . map ("! " ++) . lines


--------------------------------------------------------------------------------
-- RSYNC Command

type Host = String
type Argument = String

data RsyncPath
    = Local FilePath
    | Remote Host FilePath

rsync :: RsyncPath -> RsyncPath -> [Argument] -> Shell String
rsync p1 p2 args = run $ printf "rsync %s \"%s\" \"%s\"" (genArgs args) (genPath p1) (genPath p2)

  where
    genArgs a = intercalate " " a
    genPath (Local fp)    = fp
    genPath (Remote h fp) = h ++ ":" ++ foldr escapeSpaces "" fp
    escapeSpaces ' ' s = "\\ " ++ s
    escapeSpaces c   s   = c : s
