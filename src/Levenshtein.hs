{-# LANGUAGE OverloadedStrings #-}
module Levenshtein where

import qualified Data.Text as T
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import Data.Text.Metrics

levSelectBy :: (a -> T.Text) -> T.Text -> [a] -> [a]
levSelectBy f target options =
  map fst $
  join $
  maybeToList $
  listToMaybe $
  groupBy ((==) `on` snd) $
  sortBy (compare `on` snd) $
  zip options $ map (levenshtein target . f) options
