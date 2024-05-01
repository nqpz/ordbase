module Types
  ( ImmutableArray
  , MutableArray
  ) where

import qualified Data.Array.IArray as ArrI
import qualified Data.Array.ST as ArrST

type ImmutableArray e = ArrI.Array Int e

type MutableArray s e = ArrST.STArray s Int e
