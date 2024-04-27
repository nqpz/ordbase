{-# LANGUAGE TemplateHaskell #-}

module FileEmbedding (embedFiles) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.XML.HaXml.XmlContent (fReadXml)
import Data.Store (encode)

import qualified StoMorphology

extractLexicalEntries :: StoMorphology.LexicalResource -> [StoMorphology.LexicalEntry]
extractLexicalEntries (StoMorphology.LexicalResource _ _ _ lexicons) =
  concatMap (\(StoMorphology.Lexicon _ entries) -> entries) lexicons

embedFiles :: [FilePath] -> Q Exp
embedFiles paths = do
  mapM_ addDependentFile paths
  contents <- flip mapM paths $ \path -> do
    xml <- runIO (fReadXml path :: IO StoMorphology.LexicalResource)
    return $ extractLexicalEntries xml
  [| encode (concat contents) |]
