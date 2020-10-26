{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module GameUI
    ( gameUI
    ) where

import Data.Aeson
import Brick
import Brick.Widgets.Center                                                                                                                                               
import Brick.Widgets.Border                                                                                                                                               
import Brick.Widgets.Border.Style     
import Control.Monad(when)
import Linear.V2
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
import Brick
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
      _world    :: World
    , _settings :: Settings
    } deriving (Eq, Show, Generic)
makeLenses ''Game

instance FromJSON Game where
  parseJSON = initially $ Game initial initial
instance ToJSON Game
instance Initial Game

data Session = Session {
    _game   :: Game
  , _ending :: Ending
  } deriving (Eq, Show, Generic)
makeLenses ''Session

-- | Application specific event
data GameEvt = GameEvt
  deriving (Eq, Ord, Show)

-- | My application state 
type MyApp = App Game GameEvt WidgetName

keyToAction (KChar ' ') [] = Wait
keyToAction (KChar 'y') [] = Yell
keyToAction _           _  = Idle

gameUI :: IO ()
gameUI = restartable "game.save" $ \initialGame -> do
    Session finalGame ending <- defaultMain myApp $ Session initialGame Quit
    return (finalGame, ending)
  where
    myApp = App {
            appDraw
        ,   appChooseCursor
        ,   appHandleEvent
        ,   appAttrMap
        ,   appStartEvent
        }
    appHandleEvent  :: Session -> BrickEvent WidgetName GameEvt -> EventM WidgetName (Next Session)
    appHandleEvent s (VtyEvent (EvResize _ _            )) = continue s
    appHandleEvent s (VtyEvent (EvKey   (KChar 'q') []  )) = halt s -- quit
    appHandleEvent s (VtyEvent (EvKey   (KChar 'r') []  )) = halt $ set ending Restart s -- reload, which quits for now
    appHandleEvent s (VtyEvent (EvKey   keyName     mods)) = continue
                                                           $ over (game % world)
                                                            (updateWorld $ keyToAction keyName mods)
                                                             s
    appHandleEvent s (VtyEvent (EvPaste _               )) = continue $ set (game % world % worldMessage) "This game does not allow pasting" s
    appHandleEvent s _                                     = continue s
    
    nextTick = over (game % world % worldTime) (+1) 

    appDraw :: Session -> [Widget WidgetName]
    appDraw session = [mainWidget <=> messageWidget]
      where
        mainWidget = withBorderStyle unicode 
                   $ joinBorders
                   $ borderWithLabel (str "Map")
                   $ center $ str $ show $ view (game % world % worldTime) session
        messageWidget = withBorderStyle unicode
                      $ joinBorders
                      $ vLimit 3
                      $ borderWithLabel (str "Message")
                      $ str $ view (game % world % worldMessage) session
        
    appChooseCursor _ []    = Nothing
    appChooseCursor _ (c:_) = Just c

    appAttrMap _ = attrMap defAttr []
    appStartEvent = return
