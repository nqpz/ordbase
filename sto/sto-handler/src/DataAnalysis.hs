{-# LANGUAGE OverloadedStrings #-}

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
import Data.Char (toLower, isUpper)
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

fixId :: Text -> Text
fixId = T.pack . clean . T.unpack
  where clean ('G' : 'M' : 'U' : '_' : mainPart) = map toLower $ escape ',' '/' mainPart
        clean _ = error "Assumed all ids start with GMU_"

showAtt :: StoMorphology.Feat_att -> Text
showAtt = T.pack . clean . show
  where clean ('F' : 'e' : 'a' : 't' : '_' : 'a' : 't' : 't' : '_' : mainPart) = mainPart
        clean _ = error "Assumed all attributes to start with Feat_att_"

camelCaseToSnakeCase :: Text -> Text
camelCaseToSnakeCase = T.concatMap (T.pack . fix)
  where fix c | isUpper c = ['_', toLower c]
              | otherwise = [c]

generateWords :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generateWords = mapM_ handleEntry
  where handleEntry :: StoMorphology.LexicalEntry -> IO ()
        handleEntry (StoMorphology.LexicalEntry _attrs lexFeats _lemma wordForms _relatedForms) = do
          let wordId = fixId $ getLexFeat StoMorphology.Feat_att_id
          T.putStrLn wordId
          T.putStrLn $ getLexFeat StoMorphology.Feat_att_partOfSpeech
          T.putStrLn "-----"
          mapM_ handleWordForm wordForms
          T.putStrLn "========================================"
          T.putStrLn ""
          where getLexFeat = getFeat' lexFeats

        handleWordForm :: StoMorphology.WordForm -> IO ()
        handleWordForm (StoMorphology.WordForm mainFeats formRepresentations) = do
          forM_ mainFeats $ \feat -> do
            T.putStr $ camelCaseToSnakeCase $ showAtt $ StoMorphology.featAtt feat
            T.putStr " = "
            T.putStrLn $ camelCaseToSnakeCase $ StoMorphology.featVal feat
          forM_ formRepresentations $ \(StoMorphology.FormRepresentation reprFeats) ->
            T.putStrLn $ fromJust $ getFeat reprFeats StoMorphology.Feat_att_writtenForm
          T.putStrLn "----------------------------------------"

        getFeat :: ImmutableArray StoMorphology.Feat
                -> StoMorphology.Feat_att -> Maybe Text
        getFeat feats att = foldl' (\prev feat -> prev <|> do
                                       guard $ StoMorphology.featAtt feat == att
                                       pure $ StoMorphology.featVal feat)
                            Nothing feats

        getFeat' feats = fromJust . getFeat feats
