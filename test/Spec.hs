{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

import GameUI

import Data.Proxy
import Test.QuickCheck

import qualified Test.Vty.View
import qualified Test.Initial

import Data.Aeson
import Position
import GameUI
import World
import Entity
import Test.LessArbitrary
import qualified Data.Text as T

-- See: https://hackage.haskell.org/package/QuickCheck-2.14.1/docs/Test-QuickCheck.html#v:quickCheckAll
-- return []
-- main = $quickCheckAll

instance LessArbitrary T.Text where
  lessArbitrary = T.pack <$> lessArbitrary
instance Arbitrary T.Text where
  arbitrary = fasterArbitrary

instance LessArbitrary Value
instance Arbitrary Value where
  arbitrary = fasterArbitrary

main :: IO ()
main = do
  quickCheck   Test.Vty.View.property_testHfill
  quickCheck $ Test.Initial.property_initial_fromJSON @Pos                 Proxy
  quickCheck $ Test.Initial.property_initial_fromJSON @(Entity PlayerData) Proxy
  quickCheck $ Test.Initial.property_initial_fromJSON @World               Proxy
  quickCheck $ Test.Initial.property_initial_fromJSON @Game                Proxy