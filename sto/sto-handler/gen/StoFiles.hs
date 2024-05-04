module StoFiles
  ( morphXmlPaths
  , syntaxXmlPaths
  ) where

import Data.List (sort)
import qualified System.FilePath.Glob as Glob

morphDir :: FilePath
morphDir = "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF"

syntaxDir :: FilePath
syntaxDir = "../data/STO_syntax_v2_-_LMF_format/STO-LEXICON-SYNTAX-LMF-v2"

xmlPattern :: Glob.Pattern
xmlPattern = Glob.compile "*.xml"

morphXmlPaths :: IO [FilePath]
morphXmlPaths = sort <$> Glob.globDir1 xmlPattern morphDir

syntaxXmlPaths :: IO [FilePath]
syntaxXmlPaths = sort <$> Glob.globDir1 xmlPattern syntaxDir
