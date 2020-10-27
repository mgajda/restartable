module Main where

import GameUI
import Checkpoint
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  let saveFile | null args = "game.save"
               | otherwise = head args
  restartable saveFile GameUI.vtyUI
