{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
module GameUI where

import Data.Semigroup
import Data.Aeson
import Control.Monad(when)
import Graphics.Vty
import Graphics.Vty.Image(DisplayRegion)
import Graphics.Vty.Input.Events ( Event(..), Key(..) )
import Graphics.Vty.Attributes
import Graphics.Vty.Image
import GHC.Generics(Generic)
import Optics
import Data.Text.Optics
import Optics.Getter
import Optics.TH
import System.Posix.Process(executeFile)
import System.Environment(getArgs, getProgName)
import Control.Monad.Reader
import Checkpoint
import Linear.V2

import Initial
import World
import Position
import Entity
import Vty.View

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

keyToAction (KChar 'y') [] = Yell
keyToAction (KChar ' ') [] = Wait
-- keypad directions
keyToAction (KChar '7') [] = Move Northwest
keyToAction (KChar '8') [] = Move North
keyToAction (KChar '9') [] = Move Northeast
keyToAction (KChar '4') [] = Move West
keyToAction (KChar '5') [] = Wait
keyToAction (KChar '6') [] = Move East
keyToAction (KChar '1') [] = Move Southwest
keyToAction (KChar '2') [] = Move South
keyToAction (KChar '3') [] = Move Southeast
keyToAction  KUp        [] = Move North
keyToAction  KDown      [] = Move South
keyToAction  KLeft      [] = Move West
keyToAction  KUpLeft    [] = Move Northwest
keyToAction  KUpRight   [] = Move Northeast
keyToAction  KDownRight [] = Move Southeast
keyToAction  KDownLeft  [] = Move Southwest
keyToAction  KRight     [] = Move East
keyToAction _           _  = Idle

-- | Start VTY UI over the game.
vtyUI :: Game -> IO (Game, Ending)
vtyUI initialState = do
    cfg                  <- standardIOConfig
    vty                  <- mkVty cfg
    displaySize          <- displayBounds $ outputIface vty
    (finalState, ending) <- vtyLoop vty displaySize initialState
    shutdown vty
    return (finalState, ending)

-- TODO: should be a parameter
savefile :: FilePath
savefile  = "Game.save"

vtyLoop :: Vty -> DisplayRegion -> Game -> IO (Game, Ending)
vtyLoop vty displaySize state = do
    update vty $  display displaySize state
    e         <- nextEvent vty
    uiAction  <- eventUpdate state e
    case uiAction of
      UITerminate ending   -> return                   (state, ending)
      UIContinue  newState -> do
        -- Store state to avoid loss of data due to errors 
        encodeFile savefile newState -- FIXME: pass save file!
        vtyLoop vty displaySize newState
      UIResize    newSize  -> vtyLoop vty  newSize      state
      UISnapshot           -> do
        let snapshotFilename = savefile <> "." <> show (view (world % worldTime) state)
        encodeFile snapshotFilename state
        let newState = set (world % worldMessage)
                           ("Saved snapshot as " <> snapshotFilename) state
        vtyLoop    vty displaySize newState

eventUpdate :: Monad m => Game -> Event -> m (UIAction Game)
eventUpdate st (EvResize w h        ) = return $ UIResize    (w, h)
eventUpdate st (EvKey  (KChar 'q') _) = return $ UITerminate Quit
eventUpdate st (EvKey  (KChar 'r') _) = return $ UITerminate Restart
eventUpdate st (EvKey  (KChar 's') _) = return   UISnapshot
eventUpdate st (EvKey   keyName mods) = return $ UIContinue
                                      $ over  world
                                             (updateWorld $ keyToAction keyName mods) st
eventUpdate st  _                     = return $ UIContinue st -- ignore paste!

-- | Action of event loop:
--   either terminate the process, resize window, or continue with a new state.
data UIAction state =
    UIContinue  state
  | UISnapshot
  | UIResize    DisplayRegion
  | UITerminate Ending

-- | Cursor should be given by world only,
instance Display Game where
  display displaySize model = picForImage (bottom displaySize messageView)
                           <> display displaySize (view world model)
    where
      messageView = text' (defAttr `withBackColor` blue)
                  $ hfill displaySize $ view (world % worldMessage % packed) model

instance Display World where
  display displaySize world = display displaySize (view worldAvatar world)
                           <> picForImage
                             (center  displaySize worldView)

    where
      worldView   = string (defAttr `withForeColor` black `withBackColor` blue)
                  $ show $ view worldTime world

instance Display Avatar where
  display displaySize avatar = picForImage avatarStatus
                            <> picForImage avatarIcon
    where
      avatarStatus = right displaySize
                   $ text' (defAttr `withForeColor` yellow `withBackColor` cyan)
                   $ view (entityPos % to show % packed) avatar
      avatarIcon = translatePos (view entityPos avatar)
                 $ char (defAttr `withForeColor` yellow) '@'

translatePos :: Pos -> Image -> Image
translatePos (Pos (V2 x y)) = translate x y
