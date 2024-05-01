module ArrayUtils (ensureSingleton, arrayConcat) where

import qualified Data.Array.IArray as ArrI
import qualified DynamicArray
import qualified StoMorphology
import Types

ensureSingleton :: ImmutableArray (ImmutableArray StoMorphology.LexicalEntry)
                -> ImmutableArray StoMorphology.LexicalEntry
ensureSingleton arrays
  | ArrI.bounds arrays == (1, 1) = arrays ArrI.! 1
  | otherwise = error "unexpected amount"

arrayConcat :: [ImmutableArray StoMorphology.LexicalEntry]
            -> ImmutableArray StoMorphology.LexicalEntry
arrayConcat arrays = DynamicArray.runM' $ do
  DynamicArray.create $ sum $ map (snd . ArrI.bounds) arrays
  mapM_ (mapM_ DynamicArray.add . ArrI.elems) arrays
