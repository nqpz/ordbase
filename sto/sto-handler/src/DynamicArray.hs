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
import qualified Data.Array.ST as ArrST
import qualified Data.Array.MArray as ArrM
import qualified Data.Array.IArray as ArrI

data Array s e = Array { arrayInternal :: ArrST.STArray s Int e
                       , arrayNextIndex :: Int
                       , arrayCapacity :: Int
                       }

type M s e r = StateT (Array s e) (ST s) r

newStArray :: Int -> ST s (ArrST.STArray s Int e)
newStArray capacity = ArrM.newGenArray (1, capacity) (const (pure undefined))

create :: Int -> M s e ()
create capacity = do
  stArray <- lift $ newStArray capacity
  put $ Array { arrayInternal = stArray
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

toImmutable :: M s e (ArrI.Array Int e)
toImmutable = do
  array <- get
  arrayImmutable <- freeze (arrayInternal array)
  pure $ ArrI.genArray (1, arrayNextIndex array - 1) (arrayImmutable ArrI.!)
  where freeze :: ArrST.STArray s Int e -> M s e (ArrI.Array Int e)
        freeze = lift . ArrM.freeze

runM :: M s e r -> ST s r
runM m = evalStateT m undefined

runM' :: (forall s. M s e ()) -> ArrI.Array Int e
runM' m = runST (evalStateT (m >> toImmutable) undefined)

test :: M s Float ()
test = do
  create 3
  add 1.0
  add 2.0
  add 3.3
  add 4.0
  add 5.0
  add 6.0
  add 7.0
  add 8.0

test' :: ArrI.Array Int Float
test' = runM' test
