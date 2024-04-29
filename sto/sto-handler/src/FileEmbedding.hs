{-# LANGUAGE TemplateHaskell #-}

module FileEmbedding
  ( -- embedFiles
  -- ,
    storeFiles
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.XML.HaXml.XmlContent (fReadXml)
import qualified Data.Array.IArray as ArrI
import qualified Data.ByteString as BS
import Data.Store (encode)
import qualified DynamicArray
import Control.Monad.ST (runST)

import qualified StoMorphology

extractLexicalEntries :: StoMorphology.LexicalResource -> ArrI.Array Int StoMorphology.LexicalEntry
extractLexicalEntries (StoMorphology.LexicalResource _ _ _ lexicons) =
  (fmap (\(StoMorphology.Lexicon _ entries) -> entries) lexicons) ArrI.! 1 -- FIXME

arrayConcat :: [ArrI.Array Int StoMorphology.LexicalEntry]
            -> ArrI.Array Int StoMorphology.LexicalEntry
arrayConcat arrays = runST $ DynamicArray.runM' $ do
  DynamicArray.create $ sum $ map (snd . ArrI.bounds) arrays
  mapM_ (mapM_ DynamicArray.add . ArrI.elems) arrays


-- embedFiles :: [FilePath] -> Q Exp
-- embedFiles paths = do
--   mapM_ addDependentFile paths
--   contents <- flip mapM paths $ \path -> do
--     xml <- runIO (fReadXml path :: IO StoMorphology.LexicalResource)
--     return $ extractLexicalEntries xml
--   [| encode (concat contents) |]

storeFiles :: [FilePath] -> IO BS.ByteString
storeFiles paths = do
  contents <- flip mapM paths $ \path -> do
    xml <- fReadXml path :: IO StoMorphology.LexicalResource
    return $ extractLexicalEntries xml
  return $ encode (arrayConcat contents)
