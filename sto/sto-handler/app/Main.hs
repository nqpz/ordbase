module Main (main) where

import System.Environment (getArgs)
import qualified EmbeddedData as ED
import qualified DataAnalysis as DA

putLengths :: IO ()
putLengths = do
  putStrLn (show (DA.analyzeLength ED.morphologyLexicalEntries)
            ++ " morphology lexical entries")
  putStrLn (show (DA.analyzeLength ED.syntaxLexicalEntries)
            ++ " syntax lexical entries")
  putStrLn (show (DA.analyzeLength ED.syntaxSubcategorizationFrames)
            ++ " syntax subcategorization frames")

putData :: IO ()
putData = do
  DA.putMorphData ED.morphologyLexicalEntries
  DA.putSyntaxData ED.syntaxLexicalEntries ED.syntaxSubcategorizationFrames

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["lengths"] -> putLengths
    ["data"] -> putData
    _ -> error "unknown argument"
