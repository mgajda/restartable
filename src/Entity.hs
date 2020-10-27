{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Entity where

import Linear.V2
import Linear.Affine
import Graphics.Vty.Input.Events
import Data.Aeson
import qualified Data.Text as T
import Initial
import Optics.TH
import Optics
import GHC.Generics
import Numeric.Natural

import Position

-- * Entities with a location
data Entity a = Entity {
    _entity      :: a
  , _entityPos   :: Pos
  , _entityName  :: T.Text
  } deriving (Eq, Functor, Show, Generic, ToJSON, Initial)

instance Initial a => FromJSON (Entity a) where
  parseJSON = initially $ Entity initial initial "rabbit"

makeLenses ''Entity

-- | Move entity without checking if it is allowed.
moveEntity :: Dir -> Entity a -> Entity a
moveEntity dir = over entityPos $ movePos dir

-- * Living creatures
data Creature a = Creature {
    _hp :: Int
  , _creature :: a
  } deriving (Eq, Functor, Show, Generic)

instance Initial a => FromJSON (Creature a) where
  parseJSON = initially $ Creature 1 initial
instance ToJSON a => ToJSON (Creature a)
instance Initial a => Initial (Creature a)

makeLenses ''Creature

data Beast = Rabbit
  deriving (Eq, Show, Generic, ToJSON, Initial)

instance FromJSON Beast where
  parseJSON = initially Rabbit

data PlayerData = PlayerData
  deriving (Eq, Show, Generic, ToJSON, Initial)

instance FromJSON PlayerData where
  parseJSON = initially PlayerData

type Avatar = Entity (Creature PlayerData)

