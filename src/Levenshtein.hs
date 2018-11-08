{-# LANGUAGE OverloadedStrings #-}
module Levenshtein where

import qualified Data.Text as T
import Data.Function
import Data.List
import Data.Maybe
import Control.Monad
import Data.Text.Metrics

levSelectOne :: T.Text -> [T.Text] -> [T.Text]
levSelectOne target options =
  map fst $
  join $
  maybeToList $
  listToMaybe $
  groupBy ((==) `on` snd) $
  sortBy (flip compare `on` snd) $ zip options $ map (levenshtein target) options
