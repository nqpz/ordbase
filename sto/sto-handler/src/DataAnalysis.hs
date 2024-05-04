{-# LANGUAGE OverloadedStrings #-}

module DataAnalysis
  ( putMorphData
  , putSyntaxData
  , analyzeLength
  , generateProlog
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

fact :: Text -> [Text] -> IO ()
fact name (p0 : ps) = do
  T.putStr name
  T.putStr "("
  T.putStr p0
  forM_ ps $ \p -> do
    T.putStr ", "
    T.putStr p
  T.putStrLn ")."
fact _ [] = error "expected at least one component"

generateProlog :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generateProlog = mapM_ handleEntry
  where handleEntry :: StoMorphology.LexicalEntry -> IO ()
        handleEntry (StoMorphology.LexicalEntry _attrs lexFeats _lemma wordForms _relatedForms) = do
          let wordId = fixId $ getLexFeat StoMorphology.Feat_att_id
          fact "word"
            [ wordId
            , camelCaseToSnakeCase $ getLexFeat StoMorphology.Feat_att_partOfSpeech
            ]
          forM_ wordForms $ handleWordForm wordId
          T.putStrLn ""
          where getLexFeat = getFeat lexFeats

        handleWordForm :: Text -> StoMorphology.WordForm -> IO ()
        handleWordForm wordId (StoMorphology.WordForm mainFeats formRepresentations) = do
          forM_ mainFeats $ \feat ->
            forM_ formRepresentations $ \(StoMorphology.FormRepresentation reprFeats) ->
              fact (camelCaseToSnakeCase $ showAtt $ StoMorphology.featAtt feat)
                [ wordId
                , camelCaseToSnakeCase $ StoMorphology.featVal feat
                , getFeat reprFeats StoMorphology.Feat_att_writtenForm
                ]
          T.putStrLn ""

        getFeat feats = fromJust . getFeat' feats

        getFeat' :: ImmutableArray StoMorphology.Feat
                -> StoMorphology.Feat_att -> Maybe Text
        getFeat' feats att = foldl' (\prev feat -> prev <|> do
                                        guard $ StoMorphology.featAtt feat == att
                                        pure $ StoMorphology.featVal feat)
                             Nothing feats
