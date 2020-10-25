{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module GameUI
    ( gameUI
    ) where

import Brick
import Brick.Widgets.Center                                                                                                                                               
import Brick.Widgets.Border                                                                                                                                               
import Brick.Widgets.Border.Style     
import Linear.V2
import Graphics.Vty.Input.Events ( Event(EvResize) )
import Graphics.Vty.Attributes
import Graphics.Vty.Image

class Initial a where
    initial :: a

-- | Game world
data World = World {
    -- player :: Entity ()
    worldTime :: Int
  } deriving (Eq, Show)

instance Initial World where
    initial = World 0

-- | Identifier of UI widget
data WidgetName = Main
  deriving (Eq, Ord, Show)

data Settings = Settings
  deriving (Eq, Show)

instance Initial Settings where
    initial = Settings

-- | State of the game application
data Game = Game {
      world    :: World
    , settings :: Settings
    } deriving (Eq, Show)

instance Initial Game where
    initial = Game initial initial

-- | Application specific event
data GameEvt = GameEvt
  deriving (Eq, Ord, Show)

-- | My application state 
type MyApp = App Game GameEvt WidgetName

gameUI :: IO ()
gameUI = do
    -- TODO: autoload last game here
    finalGame <- defaultMain myApp (initial :: Game)
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


