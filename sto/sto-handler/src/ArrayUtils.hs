module ArrayUtils
  ( ensureSingleton
  , arrayConcat
  ) where

import qualified Data.Array.IArray as ArrI
import qualified DynamicArray
import Types

ensureSingleton :: ImmutableArray (ImmutableArray e)
                -> ImmutableArray e
ensureSingleton arrays
  | ArrI.bounds arrays == (1, 1) = arrays ArrI.! 1
  | otherwise = error "unexpected amount"

arrayConcat :: [ImmutableArray e]
            -> ImmutableArray e
arrayConcat arrays = DynamicArray.runM' $ do
  DynamicArray.create $ sum $ map (snd . ArrI.bounds) arrays
  mapM_ (mapM_ DynamicArray.add . ArrI.elems) arrays
