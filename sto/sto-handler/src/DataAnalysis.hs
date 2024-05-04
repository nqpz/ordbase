module DataAnalysis
  ( putMorphData
  , putSyntaxData
  , analyzeLength
  , generateWords
  ) where

import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import qualified Data.Array.IArray as ArrI
import Data.Foldable (foldl')
import Control.Applicative ((<|>))
import Control.Monad (guard, forM_)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

escape :: Char -> Char -> String -> String
escape source target = map (\c -> if c == source then target else c)

fixId :: Maybe String -> String
fixId Nothing = error "ID must always be present"
fixId (Just ('G' : 'M' : 'U' : '_' : mainPart)) = map toLower $ escape ',' '/' mainPart
fixId (Just _) = error "Assumed all ids start with GMU_"

generateWords :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generateWords = mapM_ handleEntry
  where handleEntry :: StoMorphology.LexicalEntry -> IO ()
        handleEntry (StoMorphology.LexicalEntry _attrs feats _lemma wordForms _relatedForms) = do
          let wordId = fixId (T.unpack <$> getFeat StoMorphology.Feat_att_id feats)
          putStrLn wordId
          T.putStrLn $ fromJust $ getFeat StoMorphology.Feat_att_partOfSpeech feats
          putStrLn "-----"
          mapM_ handleWordForm wordForms
          putStrLn "========================================"
          putStrLn ""

        handleWordForm :: StoMorphology.WordForm -> IO ()
        handleWordForm (StoMorphology.WordForm mainFeats formRepresentations) = do
          forM_ mainFeats $ \feat -> do
            putStr (show $ StoMorphology.featAtt feat)
            putStr " = "
            T.putStrLn $ StoMorphology.featVal feat
          forM_ formRepresentations $ \(StoMorphology.FormRepresentation reprFeats) ->
            T.putStrLn $ fromJust $ getFeat StoMorphology.Feat_att_writtenForm reprFeats
          putStrLn "----------------------------------------"

        getFeat :: StoMorphology.Feat_att -> ImmutableArray StoMorphology.Feat -> Maybe Text
        getFeat att = foldl' (\prev feat -> prev <|> do
                                 guard $ StoMorphology.featAtt feat == att
                                 pure $ StoMorphology.featVal feat)
                      Nothing
