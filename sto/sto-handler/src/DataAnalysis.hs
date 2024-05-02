module DataAnalysis
  ( putMorphData
  , putSyntaxData
  , analyzeLength
  , generateWords
  ) where

import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import qualified Data.Array.IArray as ArrI
import Data.Maybe (fromJust)
import Data.Foldable (foldl')
import Control.Applicative ((<|>))
import Control.Monad (guard)

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

analyzeLength :: ImmutableArray e -> Int
analyzeLength = snd . ArrI.bounds

generateWords :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generateWords = mapM_ handleEntry
  where handleEntry :: StoMorphology.LexicalEntry -> IO ()
        handleEntry (StoMorphology.LexicalEntry _attrs feats _lemma wordForms _relatedForms) = do
          let wordId = fromJust $ getFeat StoMorphology.Feat_att_id feats
          putStrLn wordId

        getFeat :: StoMorphology.Feat_att -> ImmutableArray StoMorphology.Feat -> Maybe String
        getFeat att = foldl' (\prev feat -> prev <|> do
                                 guard $ StoMorphology.featAtt feat == att
                                 pure $ StoMorphology.featVal feat)
                      Nothing
