{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module GameUI where

import Data.Aeson
import Graphics.Vty
import Graphics.Vty.Image(DisplayRegion)
import Graphics.Vty.Input.Events ( Event(..), Key(..) )
import Graphics.Vty.Attributes
import Graphics.Vty.Image
import GHC.Generics(Generic)
import Optics
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
viewModel vty displaySize model = do
    let worldView   = string (defAttr ` withForeColor ` green)
                    $ show $ view (world % worldTime)    model
        messageView = string (defAttr ` withBackColor ` blue)
                    $ hfill displaySize $ view (world % worldMessage) model
        pic = picForLayers [bottom displaySize messageView, center displaySize worldView]
    update vty pic

-- | Center the Vty.Image by translation
center :: DisplayRegion -> Image -> Image
center (w,h) img = translate xoff yoff img
  where
    xoff = (w-imageWidth  img) `div` 2
    yoff = (h-imageHeight img) `div` 2

-- | Put the Vty.Image at the bottom by translation
bottom :: DisplayRegion -> Image -> Image
bottom (w, h) img = translate 0 yoff img
  where
    yoff = h-imageHeight img

-- | Fill the displayed string with spaces.
hfill :: DisplayRegion -> String -> String
hfill (w, h) str = str <> replicate (w - safeWcswidth str) ' '

