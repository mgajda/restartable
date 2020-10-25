{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module GameUI
    ( gameUI
    ) where

import Data.Aeson
import Brick
import Brick.Widgets.Center                                                                                                                                               
import Brick.Widgets.Border                                                                                                                                               
import Brick.Widgets.Border.Style     
import Linear.V2
import Graphics.Vty.Input.Events ( Event(EvResize) )
import Graphics.Vty.Attributes
import Graphics.Vty.Image
import GHC.Generics(Generic)

import Initial

-- | Game world
data World = World {
    -- player :: Entity ()
    worldTime :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON World where
  parseJSON  = initially $ World 0
instance ToJSON World
instance Initial World

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
      world    :: World
    , settings :: Settings
    } deriving (Eq, Show, Generic)

instance FromJSON Game where
  parseJSON = initially $ Game initial initial
instance ToJSON Game
instance Initial Game

-- | Application specific event
data GameEvt = GameEvt
  deriving (Eq, Ord, Show)

-- | My application state 
type MyApp = App Game GameEvt WidgetName

gameUI :: IO ()
gameUI = do
    let gameFile = "game.save"
    initialGame <- restore gameFile
    -- TODO: autoload last game here
    finalGame <- defaultMain myApp initialGame
    encodeFile gameFile finalGame
    -- TODO: autosave game here
    return ()
  where
    myApp = App {
            appDraw
        ,   appChooseCursor
        ,   appHandleEvent
        ,   appAttrMap
        ,   appStartEvent
        }
    appHandleEvent  :: Game -> BrickEvent WidgetName GameEvt -> EventM WidgetName (Next Game)
    appHandleEvent s (VtyEvent (EvResize _ _)) = continue s
    appHandleEvent s _ = halt s
    
    appDraw :: Game -> [Widget WidgetName]
    appDraw game = [mainWidget]
      where
        mainWidget = withBorderStyle unicode
                   $ borderWithLabel (str "Map")
                   $ center $ str $ show $ worldTime $ world game
        
    appChooseCursor _ []    = Nothing
    appChooseCursor _ (c:_) = Just c

    appAttrMap _ = attrMap defAttr []
    appStartEvent = return


