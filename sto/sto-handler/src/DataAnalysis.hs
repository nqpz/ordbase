{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Char (toLower, isUpper, isNumber)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.State (State, modify, execState)
import qualified Data.Set as S

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

escape :: String -> String
escape = \case
  (c : cs) -> (++ concatMap clean cs) $ case c of
    _ | isNumber c -> "number_" ++ [c]
    _ -> [c]
  s -> concatMap clean s
  where clean ' ' = "_" -- should be safe even though _ also occurs on its own, but only at the end
        clean '-' = "_dash_"
        clean '\'' = "_apostrophe_"
        clean ',' = "_comma_"
        clean '[' = "_start_bracket_"
        clean ']' = "_end_bracket_"
        clean '¿' = "" -- how did this get in there?
        clean c = [c]

fixId :: Text -> Text
fixId = clean . T.unpack
  where clean ('G' : 'M' : 'U' : '_' : mainPart) = T.pack $ map toLower $ escape mainPart
        clean _ = error "Assumed all ids start with GMU_"

camelCaseToSnakeCase :: Text -> Text
camelCaseToSnakeCase = T.concatMap (T.pack . fix)
  where fix c | isUpper c = ['_', toLower c]
              | otherwise = [c]

showAtt :: StoMorphology.Feat_att -> Text
showAtt = clean . show
  where clean ('F' : 'e' : 'a' : 't' : '_' : 'a' : 't' : 't' : '_' : mainPart) = camelCaseToSnakeCase $ T.pack mainPart
        clean _ = error "Assumed all attributes to start with Feat_att_"

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
generateProlog entries = do
  generatePrologSuppressors entries
  T.putStrLn ""
  generateProlog' entries

generatePrologSuppressors :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generatePrologSuppressors entries =
  let predSet = execState (mapM handleEntry entries) S.empty
  in forM_  (S.insert ("word", 2) $ S.map (, 3 :: Int) predSet) $ \(p, n) -> do
    T.putStr ":- discontiguous "
    T.putStr p
    T.putStr "/"
    putStr $ show n
    T.putStrLn "."

  where handleEntry :: StoMorphology.LexicalEntry -> State (S.Set Text) ()
        handleEntry (StoMorphology.LexicalEntry _attrs _lexFeats _lemma wordForms _relatedForms) = mapM_ handleWordForm wordForms

        handleWordForm :: StoMorphology.WordForm -> State (S.Set Text) ()
        handleWordForm (StoMorphology.WordForm formFeats _formRepresentations) =
          mapM_ (modify . S.insert . showAtt . StoMorphology.featAtt) formFeats

generateProlog' :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generateProlog' = mapM_ handleEntry
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
        handleWordForm wordId (StoMorphology.WordForm formFeats formRepresentations) = do
          forM_ formFeats $ \feat ->
            forM_ formRepresentations $ \(StoMorphology.FormRepresentation reprFeats) ->
              let val = StoMorphology.featVal feat
              in if val /= "OBSOLETE"
                 then fact (showAtt $ StoMorphology.featAtt feat)
                        [ wordId
                        , camelCaseToSnakeCase val
                        , T.concat [ "\""
                                   , getFeat reprFeats StoMorphology.Feat_att_writtenForm
                                   , "\""
                                   ]
                        ]
                 else pure ()
          T.putStrLn ""

        getFeat feats = fromJust . getFeat' feats

        getFeat' :: ImmutableArray StoMorphology.Feat
                -> StoMorphology.Feat_att -> Maybe Text
        getFeat' feats att = foldl' (\prev feat -> prev <|> do
                                        guard $ StoMorphology.featAtt feat == att
                                        pure $ StoMorphology.featVal feat)
                             Nothing feats
