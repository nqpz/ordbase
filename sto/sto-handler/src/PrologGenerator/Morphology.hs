{-# LANGUAGE OverloadedStrings #-}
module PrologGenerator.Morphology
  ( generateProlog
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable (foldl')
import Control.Applicative ((<|>))
import Control.Monad (guard, forM_)
import Data.Maybe (fromJust)

import Types
import PrologGenerator
import qualified StoMorphology

generateProlog :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generateProlog entries = do
  generatePrologSuppressors
  generateHelpers
  T.putStrLn ""
  generateProlog' entries

generatePrologSuppressors :: IO ()
generatePrologSuppressors = forM_  [("type", 2:: Int), ("att", 4)] $ \(p, n) -> do
  T.putStr ":- discontiguous "
  T.putStr p
  T.putStr "/"
  putStr $ show n
  T.putStrLn "."

generateHelpers :: IO ()
generateHelpers = do
  printCode
    "word_id_and_type"
    [ Var "Word", Group "word" [ Var "WordId", Var "Type" ] ]
    [ Group "att" $ map Var ["_", "_", "WordId", "Word"]
    , Group "type" $ map Var [ "WordId", "Type" ]
    ]
  printCode
    "word_ids_and_types"
    [ Var "Word", Var "WordIdsAndTypes" ]
    [ Group "findall" [ Var "WordIdAndType"
                      , Group "word_id_and_type" $ map Var [ "Word", "WordIdAndType" ]
                      , Var "List"
                      ]
    , Group "sort" [ Var "List"
                   , Var "WordIdsAndTypes"
                   ]
    ]

showAtt :: StoMorphology.Feat_att -> Text
showAtt = clean . show
  where clean ('F' : 'e' : 'a' : 't' : '_' : 'a' : 't' : 't' : '_' : mainPart) = camelCaseToSnakeCase $ T.pack mainPart
        clean _ = error "Assumed all attributes to start with Feat_att_"

generateProlog' :: ImmutableArray StoMorphology.LexicalEntry -> IO ()
generateProlog' = mapM_ handleEntry
  where handleEntry :: StoMorphology.LexicalEntry -> IO ()
        handleEntry (StoMorphology.LexicalEntry _attrs lexFeats _lemma wordForms _relatedForms) = do
          let wordId = fixId $ getLexFeat StoMorphology.Feat_att_id
          fact "type"
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
                 then fact "att"
                        [ showAtt $ StoMorphology.featAtt feat
                        , camelCaseToSnakeCase val
                        , wordId
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
