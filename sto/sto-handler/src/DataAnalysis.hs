module DataAnalysis
  ( analyze
  ) where

import qualified Data.Array.IArray as ArrI

import Types

analyze :: ImmutableArray e -> Int
analyze entries = snd $ ArrI.bounds entries
