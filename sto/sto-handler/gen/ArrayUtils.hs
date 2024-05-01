{-# LANGUAGE RankNTypes #-}
module ArrayUtils
  ( ensureSingleton
  , arrayConcat
  , many
  ) where

import qualified Data.Array.IArray as ArrI
import Text.XML.HaXml.XmlContent (Parser, onFail)

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

many :: Parser t a -> Parser t (ImmutableArray a)
many = many' (DynamicArray.create 10)
  where many' :: (forall s. DynamicArray.M s a ()) -> Parser t a -> Parser t (ImmutableArray a)
        many' rs p = do
          rm <- (Just <$> p) `onFail` pure Nothing
          case rm of
            Nothing -> pure $ DynamicArray.runM' rs
            Just r -> many' (rs >> DynamicArray.add r) p
