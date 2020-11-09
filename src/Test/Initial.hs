{-# LANGUAGE ScopedTypeVariables   #-}
-- | This module describes values that
--   are initialized from a list of _all optional_ Aeson.Values.
--   Initialization should be fully generic by default.
module Test.Initial where

import Data.Aeson ( fromJSON, Result(Success), Value )
import Data.Proxy ( Proxy(..) )

import Control.Restartable.Initial

-- * Tests for Initial class:
-- | Check that we get some value no matter what.
property_initialFromJSON :: Initial a => Proxy a -> Value -> Bool
property_initialFromJSON (Proxy :: Proxy a) = isValid . (fromJSON :: Value -> Result a)
  where
    isValid (Success _) = True
    isValid _           = False
