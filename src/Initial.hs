{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PartialTypeSignatures #-} -- Development only!
module Initial where

-- ^ This module describes values that
--   are initialized from a list of _all optional_ Aeson.Values.
--   Initialization should be fully generic by default.

import Control.Applicative((<|>))
import Data.Aeson.Types
import Data.Aeson
    ( decodeFileStrict
    , fromJSON
    , parseJSON
    , FromJSON
    , Result(Success, Error)
    , Value(Null) )
import           Data.Maybe(fromMaybe, fromJust, isJust)
import GHC.Generics ( Generic(Rep) )
import System.IO.Error

-- | Special class for values that should give initial value when JSON parse fails.
class (FromJSON a
      ,ToJSON   a)
   =>  Initial  a

-- | Initial value
initial :: Initial a => a
initial  = case fromJSON Null of
  Error err -> error "Failed to initialize from empty JSON!"
  Success a -> a

-- | Implements FromJSON with a fixed initialization.
--initializeWith :: (Generic a, GFromJSON Zero (Rep a)) => a -> Value -> Parser a
initially :: (Generic a, GFromJSON Zero (Rep a)) => a -> Value -> Parser a
initially x v = genericParseJSON robustOptions v <|> pure x

-- | JSON encoding options that are more robust to datatype migration.
robustOptions :: Options
robustOptions =
  defaultOptions {
    omitNothingFields     = True
  , allNullaryToStringTag = False
  }

