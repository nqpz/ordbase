module DataAnalysis
  ( putMorphData
  , putSyntaxData
  , analyze
  ) where

import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import qualified Data.Array.IArray as ArrI

import Types
import qualified StoMorphology
import qualified StoSyntax

emptyArray :: ImmutableArray e
emptyArray = ArrI.listArray (1, 0) []

putMorphData :: ImmutableArray StoMorphology.LexicalEntry
             -> IO ()
putMorphData entries = do
  hPutXml stdout False $ StoMorphology.Lexicon emptyArray entries
  hFlush stdout

putSyntaxData :: ImmutableArray StoSyntax.LexicalEntry
              -> ImmutableArray StoSyntax.SubcategorizationFrame
              -> IO ()
putSyntaxData entries frames = do
  hPutXml stdout False $ StoSyntax.Lexicon emptyArray entries frames
  hFlush stdout

analyze :: ImmutableArray e -> Int
analyze entries = snd $ ArrI.bounds entries
