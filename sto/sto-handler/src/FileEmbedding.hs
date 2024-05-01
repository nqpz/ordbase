{-# LANGUAGE TemplateHaskell #-}

module FileEmbedding
  ( morphsToString
  , embedMorphs
  , syntaxsToStrings
  , embedSyntaxs
  ) where

import Language.Haskell.TH
import Text.XML.HaXml.XmlContent (fReadXml)
import Data.ByteString (ByteString)
import Data.Store (encode)
import ArrayUtils

import qualified StoMorphology
import qualified StoSyntax

morphsToString :: [FilePath] -> IO ByteString
morphsToString morphPaths = do
  contents <- flip mapM morphPaths $ \path -> do
    xml <- fReadXml path :: IO StoMorphology.LexicalResource
    pure $ StoMorphology.extractLexicalEntries xml
  pure $ encode $ arrayConcat contents

embedMorphs :: IO [FilePath] -> Q Exp
embedMorphs paths = runIO $ do
  s <- morphsToString =<< paths
  [| s |]

syntaxsToStrings :: [FilePath] -> IO (ByteString, ByteString)
syntaxsToStrings syntaxPaths = do
  contents <- flip mapM syntaxPaths $ \path -> do
    xml <- fReadXml path :: IO StoSyntax.LexicalResource
    pure (StoSyntax.extractLexicalEntries xml, StoSyntax.extractSubcategorizationFrames xml)
  let (lexicalEntries, subCategorizationFrames) = unzip contents
  pure (encode (arrayConcat lexicalEntries),
        encode (arrayConcat subCategorizationFrames))

embedSyntaxs :: IO [FilePath] -> Q Exp
embedSyntaxs paths = runIO $ do
  s <- syntaxsToStrings =<< paths
  [| s |]
