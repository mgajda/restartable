{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module GameUI where

import Data.Aeson
import Control.Monad(when)
import Graphics.Vty
import Graphics.Vty.Image(DisplayRegion)
import Graphics.Vty.Input.Events ( Event(..), Key(..) )
import Graphics.Vty.Attributes
import Graphics.Vty.Image
import GHC.Generics(Generic)
import Optics
import Optics.Getter
import Optics.TH
import System.Posix.Process(executeFile)
import System.Environment(getArgs, getProgName)
import Control.Monad.Reader
import Checkpoint

import Initial
import World

-- | Identifier of UI widget
data WidgetName = Main
  deriving (Eq, Ord, Show, Generic)

data Settings = Settings
  deriving (Eq, Show, Generic)

instance FromJSON Settings where
  parseJSON = initially Settings
instance ToJSON Settings
instance Initial Settings

-- | State of the game application
data Game = Game {
      _world         :: World
    , _settings      :: Settings
    } deriving (Eq, Show, Generic)
makeLenses ''Game

instance FromJSON Game where
  parseJSON = initially $ Game initial initial
instance ToJSON Game
instance Initial Game

keyToAction (KChar ' ') [] = Wait
keyToAction (KChar 'y') [] = Yell
keyToAction _           _  = Idle


-- | Start VTY UI over the game.
vtyUI :: Game -> IO (Game, Ending)
vtyUI initialState = do
    cfg         <- standardIOConfig
    vty         <- mkVty cfg
    displaySize <- displayBounds $ outputIface vty
    (finalState, ending) <- vtyLoop vty displaySize initialState
    shutdown vty
    return (finalState, ending)

vtyLoop :: Vty -> DisplayRegion -> Game -> IO (Game, Ending)
vtyLoop vty displaySize state = do
    viewModel vty displaySize state
    e        <- nextEvent vty
    uiAction <- eventUpdate state e
    case uiAction of
      UITerminate ending   -> return                   (state, ending)
      UIContinue  newState -> vtyLoop vty  displaySize  newState
      UIResize    newSize  -> vtyLoop vty  newSize      state
  
eventUpdate :: Monad m => Game -> Event -> m (UIAction Game)
eventUpdate st (EvResize w h        ) = return $ UIResize    (w, h)
eventUpdate st (EvKey  (KChar 'q') _) = return $ UITerminate Quit
eventUpdate st (EvKey  (KChar 'r') _) = return $ UITerminate Restart
eventUpdate st (EvKey   keyName mods) = return $ UIContinue
                                      $ over  world
                                             (updateWorld $ keyToAction keyName mods) st
eventUpdate st  _                     = return $ UIContinue          st -- ignore paste!

data UIAction state =
    UIContinue  state
  | UIResize    DisplayRegion
  | UITerminate Ending

viewModel :: Vty -> DisplayRegion -> Game -> IO ()
viewModel vty (w, h) model = do
    let line0 = string (defAttr ` withForeColor ` green)
              $ show $ view (world % worldTime) model
        line1 = string (defAttr ` withBackColor ` blue) "second line"
        img = line0 <-> line1
        pic = picForImage img
    update vty pic
