{-# LANGUAGE TemplateHaskell #-}

module FileEmbedding
  ( -- embedFiles
  -- ,
    toLexiconString
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.XML.HaXml.XmlContent (fReadXml)
import qualified Data.Array.IArray as ArrI
import qualified Data.ByteString as BS
import Data.Store (encode)
import qualified DynamicArray
import Types
import ArrayUtils

import qualified StoMorphology

-- embedFiles :: [FilePath] -> Q Exp
-- embedFiles paths = do
--   mapM_ addDependentFile paths
--   contents <- flip mapM paths $ \path -> do
--     xml <- runIO (fReadXml path :: IO StoMorphology.LexicalResource)
--     return $ extractLexicalEntries xml
--   [| encode (concat contents) |]

toLexiconString :: [FilePath] -> IO BS.ByteString
toLexiconString paths = do
  contents <- flip mapM paths $ \path -> do
    xml <- fReadXml path :: IO StoMorphology.LexicalResource
    return $ StoMorphology.extractLexicalEntries xml
  return $ encode (arrayConcat contents)
