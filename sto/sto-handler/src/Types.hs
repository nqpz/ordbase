module Types
  ( ImmutableArray
  , ImmutableArray1
  , MutableArray
  ) where

import qualified Data.Array.IArray as ArrI
import qualified Data.Array.ST as ArrST

type ImmutableArray e = ArrI.Array Int e

-- Really this means that there must be at least one element in the XML, but the
-- XML files we have trivially fulfil this invariant, so we don't need to check
-- it.
type ImmutableArray1 e = ImmutableArray e

type MutableArray s e = ArrST.STArray s Int e
