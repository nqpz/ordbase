module Main (main) where

import System.Environment (getArgs)
import qualified EmbeddedData as ED
import qualified DataAnalysis as DA
import qualified PrologGenerator.Morphology as PGM
import qualified PrologGenerator.Syntax as PGS
import qualified StoFiles

putFiles :: IO ()
putFiles = do
  putStrLn "Morphology XML files:"
  mapM_ putStrLn =<< StoFiles.morphXmlPaths
  putStrLn ""
  putStrLn "Syntax XML files:"
  mapM_ putStrLn =<< StoFiles.syntaxXmlPaths

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
    ["files"] -> putFiles
    ["lengths"] -> putLengths
    ["data"] -> putData
    _ -> error "unknown argument"
