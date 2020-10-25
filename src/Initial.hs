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

import Data.Aeson
    ( decodeFileStrict
    , fromJSON
    , parseJSON
    , FromJSON
    , Result(Success, Error)
    , Value(Null) )
import           Data.Maybe(fromMaybe, fromJust, isJust)
import Data.Proxy


-- | Special class for values that should give initial value when JSON parse fails.
class FromJSON a
   => Initial  a where
  -- | Initial value
  initial :: a
  initial  = case fromJSON Null of
               Error err -> error "Failed to initialize from empty JSON!"
               Success a -> a


-- | Reload old value from file, or initialize it from scratch if file is not present.
restore :: FromJSON a => FilePath -> IO a
restore filepath = do
  result <- decodeFileStrict filepath
  case result of
    Nothing -> error "Cannot load initial value!"
    Just  x -> return x
