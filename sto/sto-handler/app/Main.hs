module Main (main) where

import qualified EmbeddedData as ED
import qualified DataAnalysis as DA

main :: IO ()
main = do
  putStrLn (show (DA.analyze ED.morphologyLexicalEntries) ++ " morphology lexical entries")
  putStrLn (show (DA.analyze ED.syntaxLexicalEntries) ++ " syntax lexical entries")
  putStrLn (show (DA.analyze ED.syntaxSubcategorizationFrames) ++ " syntax subcategorization frames")
