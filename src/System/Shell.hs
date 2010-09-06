{-# LANGUAGE GeneralizedNewtypeDeriving,
    TypeSynonymInstances, MultiParamTypeClasses
    #-}

module System.Shell
    ( Shell
    , Priv (..)
    , shell
    , run
    , runPriv
    ) where


import System.Exit
import System.IO

import System.Process hiding (shell)
import qualified System.Process as P

import Control.Monad.Error


-- | The 'Shell' monad, a wrapper over IO that captures failure in an error
-- transformer.
newtype Shell a = Shell { runShell :: ErrorT String IO a }
  deriving (Functor, Monad, MonadIO)

-- | The 'Priv' monad, a shell monad for commands requiring root privledges.
-- Let's us distinguish such command statically, on the type level.
--
-- To run something in the Priv monad, use 'priv'.
newtype Priv a = Priv { priv :: Shell a }
    deriving (Functor, Monad, MonadIO)

-- Rather than just derive error handling, we'll roll our own that propagates
-- shell failures into errors.
instance MonadError String Shell where
    throwError     = error . ("Shell failed: " ++) . (unlines . map ("! " ++) . lines)
    catchError (Shell errt) h = Shell $ errt `catchError` (runShell . h)

instance MonadError String Priv  where
    throwError     = error . ("Priv failed: "++) . (unlines . map ("! " ++) . lines)
    catchError (Priv sh) h = Priv $ sh `catchError` (priv . h)


-- | Run a normal shell command as the user. Return either a result or an error
-- value
shell :: Shell a -> IO (Either String a)
shell = runErrorT . runShell

-- | Convenient wrapper for IO
io :: IO a -> Shell a
io = liftIO

io' :: IO a -> ErrorT String IO a
io' = runShell . io

-- | Run a shell command, wrapping any errors in ErrorT and wait for its result
run :: String -> Shell String
run s = Shell $ do

    (_, Just hout, Just herr, ph) <- io' $
        createProcess (P.shell s) { std_out = CreatePipe, std_err = CreatePipe }

    ec <- io' $ waitForProcess ph
    case ec of
         ExitSuccess   -> io' $ hGetContents hout
         ExitFailure _ -> throwError =<< io' (hGetContents herr)

-- | Run a privileged command, requiring sudo access. Return any output
runPriv :: String -> Priv String
runPriv = Priv . run . ("/usr/bin/sudo " ++)
