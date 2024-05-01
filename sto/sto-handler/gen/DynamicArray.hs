{-# LANGUAGE RankNTypes #-}
module DynamicArray
  ( M
  , runM
  , runM'
  , create
  , add
  , toImmutable
  ) where

import Control.Monad.State
import Control.Monad.ST (ST, runST)
import qualified Data.Array.MArray as ArrM
import qualified Data.Array.IArray as ArrI
import Types

data DynamicArray s e = DynamicArray { arrayInternal :: MutableArray s e
                                     , arrayNextIndex :: Int
                                     , arrayCapacity :: Int
                                     }

type M s e r = StateT (DynamicArray s e) (ST s) r

newStArray :: Int -> ST s (MutableArray s e)
newStArray capacity = ArrM.newGenArray (1, capacity) (const (pure undefined))

create :: Int -> M s e ()
create capacity = do
  stArray <- lift $ newStArray capacity
  put $ DynamicArray { arrayInternal = stArray
                     , arrayNextIndex = 1
                     , arrayCapacity = capacity
                     }

add :: e -> M s e ()
add e = do
  array <- get
  let i = arrayNextIndex array
  let capacity = arrayCapacity array
  array' <- lift
    (if i <= capacity
     then pure array
     else do
        let capacity' = capacity * 2
        stArray <- newStArray capacity'
        mapM_ (\j -> do
                  old <- ArrM.readArray (arrayInternal array) j
                  ArrM.writeArray stArray j old) [1..i - 1]
        pure array { arrayInternal = stArray
                   , arrayCapacity = capacity'
                   })
  lift $ ArrM.writeArray (arrayInternal array') i e
  put array' { arrayNextIndex = i + 1 }

toImmutable :: M s e (ImmutableArray e)
toImmutable = do
  array <- get
  arrayImmutable <- freeze $ arrayInternal array
  pure $ ArrI.genArray (1, arrayNextIndex array - 1) (arrayImmutable ArrI.!)
  where freeze :: MutableArray s e -> M s e (ImmutableArray e)
        freeze = lift . ArrM.freeze

runM :: M s e r -> ST s r
runM m = evalStateT m undefined

runM' :: (forall s. M s e ()) -> ImmutableArray e
runM' m = runST (evalStateT (m >> toImmutable) undefined)
