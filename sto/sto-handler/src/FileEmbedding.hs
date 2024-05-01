{-# LANGUAGE TemplateHaskell #-}

module FileEmbedding
  ( morphsToLexiconString
  , embedMorphs
  ) where

import Language.Haskell.TH
import Text.XML.HaXml.XmlContent (fReadXml)
import Data.ByteString (ByteString)
import Data.Store (encode)
import ArrayUtils

import qualified StoMorphology

morphsToLexiconString :: [FilePath] -> IO ByteString
morphsToLexiconString morphPaths = do
  contents <- flip mapM morphPaths $ \path -> do
    xml <- fReadXml path :: IO StoMorphology.LexicalResource
    return $ StoMorphology.extractLexicalEntries xml
  return $ encode (arrayConcat contents)

embedMorphs :: [FilePath] -> Q Exp
embedMorphs morphPaths = [| runIO (morphsToLexiconString morphPaths) |]
