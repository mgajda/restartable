{-# LANGUAGE DeriveGeneric #-}
-- | Make a checkpointable session
--   provided that session state implements `Initial`.
module Checkpoint(restartable, Ending(..)) where

import Data.Aeson
import GHC.Generics(Generic)
import System.Posix.Process(executeFile)
import System.Environment(getArgs, getProgName)
import System.IO.Error
import Control.Monad(when)

import Initial

-- | Reload old value from file, or initialize it from scratch if file is not present.
restore :: Initial a => FilePath -> IO a
restore filepath = do
  result <- decodeFileStrict filepath `catchIOError` (\_ -> return $ Just initial)
  case result of
    Nothing -> error "Cannot load initial value!"
    Just  x -> return x
-- | Way in which game terminated.

data Ending = Quit     -- exit application
            | Restart  -- save state, and resume with a new executable on the same path
            | Continue -- save state, and resume execution
  deriving (Eq, Ord, Show, Generic)

-- | Restart current executable with the same arguments.
restartExecutable :: IO ()
restartExecutable = do
  args    <- getArgs
  exeName <- getProgName
  executeFile exeName True args Nothing
  -- Never returns

-- | Should be used to wrap `main`.
restartable ::  Initial  a
            =>  FilePath
            -> (a -> IO (a, Ending))
            ->       IO  ()
restartable savefile app = do
    initialState <- restore savefile
    go initialState
  where
    go initialState = do
      (finalState, ending) <- app initialState
      encodeFile savefile finalState
      case ending of
        -- Reload code (after recompile).
        Restart  -> restartExecutable
        -- Resume execution after saving the state
        Continue -> go finalState
        -- Exit application
        Quit     -> return ()    

