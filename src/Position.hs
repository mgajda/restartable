{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | Position and direction
module Position where

import Data.Aeson
import Linear.V2
import Optics
import GHC.Generics

import Initial

-- | World location
newtype Pos = Pos { _pos :: V2 Int }
  deriving (Eq, Ord, Num, Read, Generic)

instance Show Pos where
  show (Pos (V2 x y)) = showLL 'N' 'S' y <> ", " <> showLL 'W' 'E' x

-- | Show a single longitude/latitude coordinate.
showLL :: (Ord a, Num a, Show a) => Char -> Char -> a -> String
showLL negChar posChar coord | coord <  0 = show coord <> ['°', negChar]
showLL negChar posChar coord | coord >  0 = show coord <> ['°', posChar]
showLL negChar posChar coord | coord == 0 = show coord <> ['°', ' '    ]

-- | Note that we do not want generic instance for V2, since initial values may be different.
instance FromJSON Pos where
  parseJSON = fmap Pos . initially (V2 0 0)
instance ToJSON Pos where
  toJSON = genericToJSON defaultOptions . _pos
instance Initial Pos

makeLenses ''Pos

-- | Direction
data Dir = Northwest
         | North
         | Northeast
         | East
         | Southeast
         | South
         | Southwest
         | West
  deriving (Eq, Ord, Show, Read, Generic)

-- | Absolute directions in square world.
directionVector          :: Dir -> Pos
directionVector Northwest = Pos $ V2 (-1) (-1)
directionVector North     = Pos $ V2   0  (-1)
directionVector Northeast = Pos $ V2   1  (-1)
directionVector East      = Pos $ V2   1    0
directionVector Southeast = Pos $ V2   1    1
directionVector South     = Pos $ V2   0    1
directionVector Southwest = Pos $ V2 (-1)   1
directionVector West      = Pos $ V2 (-1)   0

-- | Move position in a given direction by one square.
movePos  :: Dir -> Pos -> Pos
movePos d = over pos (+view pos (directionVector d))