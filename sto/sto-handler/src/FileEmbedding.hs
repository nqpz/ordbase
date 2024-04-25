{-# LANGUAGE TemplateHaskell #-}

module FileEmbedding (embedFiles) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.XML.HaXml.XmlContent (fReadXml)

import qualified StoMorphology

extractLexicalEntries :: StoMorphology.LexicalResource -> [StoMorphology.LexicalEntry]
extractLexicalEntries (StoMorphology.LexicalResource _ _ _ lexicons) =
  concatMap (\(StoMorphology.Lexicon _ entries) -> entries) lexicons

embedFiles :: [FilePath] -> Q Exp
embedFiles paths = do
  contents <- mapM (\path -> runIO (fReadXml path :: IO StoMorphology.LexicalResource)) paths
  mapM_ addDependentFile paths
  [| concatMap extractLexicalEntries contents |]
