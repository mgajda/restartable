{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PartialTypeSignatures #-} -- Development only!
module Test.Initial where

-- ^ This module describes values that
--   are initialized from a list of _all optional_ Aeson.Values.
--   Initialization should be fully generic by default.

import Data.Aeson
import Data.Aeson.Types
import Data.Proxy

import Control.Restartable.Initial

-- * Tests for Initial class:
property_initial_fromJSON :: Initial a => Proxy a -> Value -> Bool
property_initial_fromJSON (Proxy :: Proxy a) = isValid . (fromJSON :: Value -> Result a)
  where
    isValid (Success a) = True
    isValid _           = False
