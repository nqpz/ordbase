{-# LANGUAGE TemplateHaskell #-}

module FileEmbedding (embedFile) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.XML.HaXml.XmlContent (fReadXml)

import qualified StoMorphology

embedFile :: FilePath -> Q Exp
embedFile path = do
  contents <- runIO (fReadXml path :: IO StoMorphology.LexicalResource)
  addDependentFile path
  [| contents |]
