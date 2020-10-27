{-# LANGUAGE ViewPatterns #-}
module Test.Vty.View where

import Vty.View
import Graphics.Text.Width
import qualified Data.Text as T
import Data.Char
import Test.QuickCheck

property_testHfill :: Int -> T.Text -> Property
property_testHfill i (T.filter acceptableChar -> txt) =
      acceptableText txt
  && (T.null txt || isPrint (T.last txt))
  ==> T.length (hfill (i, 1) txt) == max i (safeWctwidth txt)

