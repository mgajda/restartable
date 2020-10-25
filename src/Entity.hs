module Entity where

import Linear.V2
import Graphics.Vty.Input.Events

newtype Pos = Pos (V2 Int)

data Entity a = Entity {
      _entityInner :: a
    , _entityPos :: Pos
    }