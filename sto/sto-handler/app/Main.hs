module Main (main) where

import qualified EmbeddedData as ED
import qualified DataAnalysis as DA

main :: IO ()
main = do
  putStrLn (show (DA.analyzeLength ED.morphologyLexicalEntries) ++ " morphology lexical entries")
  putStrLn (show (DA.analyzeLength ED.syntaxLexicalEntries) ++ " syntax lexical entries")
  putStrLn (show (DA.analyzeLength ED.syntaxSubcategorizationFrames) ++ " syntax subcategorization frames")
