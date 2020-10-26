module Main where

import GameUI
import Checkpoint
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then restartable "game.save" GameUI.vtyUI
    else restartable (head args) GameUI.vtyUI
