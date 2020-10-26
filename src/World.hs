{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Entire game world, and player actions.
module World( World
            , Action(..)
            , updateWorld
            -- lenses
            , worldTime
            , worldMessage
            ) where

import Data.Aeson
import Graphics.Vty.Input.Events ( Event(..), Key(..) )
import Graphics.Vty.Attributes
import Graphics.Vty.Image
import GHC.Generics(Generic)
import Optics
import Optics.Getter
import Optics.TH
import System.Posix.Process(executeFile)
import System.Environment(getArgs, getProgName)

import Initial

-- | Game world
data World = World {
    -- player :: Entity ()
    _worldTime :: Int
  , _worldMessage :: String
  } deriving (Eq, Show, Generic)
makeLenses ''World

-- | Type of events used to signal passage of time.
data WorldTick = Tick
               | Instant

instance FromJSON World where
  parseJSON  = initially $ World 0 "Hello"
instance ToJSON World
instance Initial World

-- | All possible player actions.
data Action =
    Wait -- wait for one turn
  | Yell -- Yell around
  | Idle -- nothing happens

nextTick :: World -> World
nextTick = over worldTime (+1)

updateWorld :: Action -> World -> World
updateWorld Idle = id
updateWorld Wait = nextTick
                 . set worldMessage " "
updateWorld Yell = nextTick
                 . set worldMessage "Aaaargh!!!"

