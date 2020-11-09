{-# LANGUAGE FlexibleContexts      #-}
-- | This module describes values that
--   are initialized from a list of _all optional_ Aeson.Values.
--   Initialization should be fully generic by default.
module Control.Restartable.Initial where

import Control.Applicative((<|>))
import Data.Aeson.Types
import GHC.Generics ( Generic(Rep) )

-- | Special class for values that should give initial value when JSON parse fails.
class (FromJSON a
      ,ToJSON   a)
   =>  Initial  a

-- | Initial value
initial :: Initial a => a
initial  = case fromJSON Null of
  Error   _ -> error "Failed to initialize from empty JSON!"
  Success a -> a

-- | Implements FromJSON with a fixed initialization.
--initializeWith :: (Generic a, GFromJSON Zero (Rep a)) => a -> Value -> Parser a
initially    :: (Generic a, GFromJSON Zero (Rep a))
             => a -> Value -> Parser a
initially x v = genericParseJSON robustOptions v <|> pure x

-- | JSON encoding options that are more robust to datatype migration.
robustOptions :: Options
robustOptions =
  defaultOptions {
    omitNothingFields     = True
  , allNullaryToStringTag = False
  }
