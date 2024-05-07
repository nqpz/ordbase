module Main (main) where

import System.Environment (getArgs)
import qualified EmbeddedData as ED
import qualified DataAnalysis as DA
import qualified PrologGenerator.Morphology as PGM
import qualified PrologGenerator.Syntax as PGS

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
    ["generateMorphologyProlog"] -> PGM.generateProlog ED.morphologyLexicalEntries
    ["generateSyntaxProlog"] -> PGS.generateProlog ED.syntaxLexicalEntries ED.syntaxSubcategorizationFrames
    ["lengths"] -> putLengths
    ["data"] -> putData
    _ -> error "unknown argument"
