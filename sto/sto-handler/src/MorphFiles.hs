module MorphFiles
  ( morphXmlPaths
  ) where

import qualified System.FilePath.Glob as Glob

morphDir :: FilePath
morphDir = "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF"

xmlPattern :: Glob.Pattern
xmlPattern = Glob.compile "*.xml"

morphXmlPaths :: IO [FilePath]
morphXmlPaths = pure [morphDir ++ "/STO_LMF_morphology_noun_q_jan2013.xml"] -- for testing
-- morphXmlPaths = Glob.globDir1 xmlPattern morphDir
