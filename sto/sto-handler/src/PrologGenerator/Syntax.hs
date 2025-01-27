{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module PrologGenerator.Syntax
  ( generateProlog
  ) where

import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromJust)
import Data.Foldable (foldl')
import Control.Applicative ((<|>))
import Control.Monad (guard, forM_)
import qualified Data.Array.IArray as ArrI

import Types
import PrologGenerator
import qualified StoSyntax

generateProlog :: ImmutableArray StoSyntax.LexicalEntry
               -> ImmutableArray StoSyntax.SubcategorizationFrame
               -> IO ()
generateProlog entries frames = do
  T.putStrLn ":- style_check(-singleton)." -- TEMPORARY
  generatePrologSuppressors [("noun_group", 2), ("adjective_group", 4)]
  T.putStrLn "eq(X, X)." -- means I can write less code below
  generateEntries entries
  generateFrames frames

fixFrame :: Text -> Text
fixFrame frame = T.concat [ "kind_", T.replace "-" "_" frame]

getFeat feats = fromJust . getFeat' feats

getFeat' :: ImmutableArray StoSyntax.Feat
         -> StoSyntax.Feat_att -> Maybe Text
getFeat' feats att = foldl' (\prev feat -> prev <|> do
                                guard $ StoSyntax.featAtt feat == att
                                pure $ StoSyntax.featVal feat)
                     Nothing feats

generateEntries :: ImmutableArray StoSyntax.LexicalEntry -> IO ()
generateEntries = mapM_ handleEntry
  where handleEntry :: StoSyntax.LexicalEntry -> IO ()
        handleEntry (StoSyntax.LexicalEntry _attrs lexFeats _lemma behaviours) = do
          let wordId = fixId $ getLexFeat StoSyntax.Feat_att_id
          forM_ behaviours $ \(StoSyntax.SyntacticBehaviour behaviourAttrs _behaviourFeats) ->
            let frames = T.splitOn " " $ fromJust $ StoSyntax.syntacticBehaviourSubcategorizationFrames behaviourAttrs
            in forM_ frames $ \frame ->
              fact "phrase_kind"
                [ wordId
                , fixFrame frame
                ]
          where getLexFeat = getFeat lexFeats

type ArgumentWithIndex = (Int, ImmutableArray StoSyntax.Feat)

fixme :: Text -> [(Text, [Exp])]
fixme fun = [ (T.concat [ "\"FIXME-", fun, "\"" ], []) ] -- FIXME

-- Relevant word attributes: case, definiteness, (maybe reflexiveVoice?)
generateNoun :: Text -> [ImmutableArray StoSyntax.Feat] -> IO ()
generateNoun kind args = do
  printCode
    "noun_group"
    [ Var "Definiteness", Var (T.pack ("[" ++ L.intercalate ", " (map T.unpack words) ++ "]")) ]
    (concat wordReqs)
  where (words, wordReqs) = unzip $ concatMap toWord $ zip [1..] args
        kindWordReqs = [ Group "phrase_kind" $ map Var ["KindWordId", kind]
                       , Group "att" $ map Var ["case", "unspecified", "KindWordId", "KindWord"]
                       , Group "att" $ map Var ["definiteness", "Definiteness", "KindWordId", "KindWord"]
                       ]

        -- Noun syntactic functions:
        -- - clausalComplement
        -- - externalComplement
        -- - prepositionalComplement
        -- - relationalGenitive
        -- - somPrepComplement
        -- - specifierNoun
        toWord :: (Int, ImmutableArray StoSyntax.Feat) -> [(Text, [Exp])]
        toWord (i, feats) = case getFeat' feats StoSyntax.Feat_att_syntacticFunctionType of
          -- FIXME: refactor shared parts
          Just "prepositionalComplement" ->
            case getFeat' feats StoSyntax.Feat_att_ppComplementLabel of
              Just "NP" ->
                let w = T.pack ("Word" ++ show i)
                    wid = T.pack ("WordId" ++ show i)
                    results =
                      [ (w, [ Group "word_id_and_type"
                              [ Var w
                              , Group "word" $ map Var [ wid, "common_noun" ] -- TODO: also other categories than common_noun?
                              ]
                            , Group "att" $ map Var [ "case", "unspecified", wid, w ]
                            ])
                      ]
                    results' = (T.concat [ "\"", getFeat feats StoSyntax.Feat_att_introducer, "\"" ], []) : results
                    results'' = if i == 1 then ("KindWord", kindWordReqs) : results' else results'
                in results''
              -- Just "infinitiveWithoutControl" ->
              --   [ "\"at\"" main_verb ...
              _ -> fixme "not-np"
          Just "relationalGenitive" ->
            case getFeat' feats StoSyntax.Feat_att_syntacticConstituentLabel of
              Just "NP" ->
                let w = T.pack ("Word" ++ show i)
                    wid = T.pack ("WordId" ++ show i)
                    results =
                      [ (w, [ Group "word_id_and_type"
                              [ Var w
                              , Group "word" $ map Var [ wid, "common_noun" ] -- TODO: also other categories than common_noun? Maybe just don't search for this at all since we're also looking for genitive_case?
                              ]
                            , Group "att" $ map Var [ "case", "genitive_case", wid, w ]
                            ])
                      ]
                    results' = if i == 1 then results ++ [("KindWord", Group "eq" [Var "Definiteness", Var "indefinite"] : kindWordReqs)] else results
                in results'
              _ -> fixme "not-np"
          Just fun ->
            fixme fun
          _ ->
            fixme "unknown"

-- Relevant word attributes: degree, grammatical_gender, grammatical_number
generateAdjective :: Text -> [ImmutableArray StoSyntax.Feat] -> IO ()
generateAdjective kind args = do
  printCode
    "adjective_group"
    [ Var "Degree", Var "GrammaticalGender", Var "GrammaticalNumber", Var (T.pack ("[" ++ L.intercalate ", " (map T.unpack words) ++ "]")) ]
    (concat wordReqs)
  where (words, wordReqs) = unzip $ concatMap toWord $ zip [1..] args

        -- Adjective syntactic functions:
        -- - clausalComplement
        -- - externalComplement
        -- - formalComplement
        -- - nominalComplement
        -- - prepositionalComplement
        -- - somPrepComplement
        toWord :: (Int, ImmutableArray StoSyntax.Feat) -> [(Text, [Exp])]
        toWord (i, feats) = case getFeat' feats StoSyntax.Feat_att_syntacticFunctionType of
          Just "externalComplement" ->
            case getFeat' feats StoSyntax.Feat_att_syntacticConstituentLabel of
              Just "NP" ->
                let w = T.pack ("Word" ++ show i)
                    -- wid = "_" -- T.pack ("WordId" ++ show i)
                    adj = T.pack ("Adj" ++ show i)
                    results =
                      -- TODO: Okay to not explicitly first look at word class?

                      -- TODO: Currently hardcoded to be unspecified, but the
                      -- value seems to always be
                      -- "nominativeCase,accusativeCase,unspecified", so we
                      -- should also accept the two other cases. Requires uglier
                      -- Prolog code.
                      [ (w, [ Group "att" $ map Var [ "case", "unspecified", "_", w ]
                            , Group "att" $ map Var [ "grammatical_gender", "GrammaticalGender", "_", w ]
                            , Group "att" $ map Var [ "grammatical_number", "GrammaticalNumber", "_", w ]
                            ])
                      , ("\"er\"", []) -- TODO: Support past "var" as well, etc.
                      , (adj, [ Group "att" $ map Var [ "degree", "Degree", "_", adj ]
                              , Group "att" $ map Var [ "grammatical_gender", "GrammaticalGender", "_", adj ]
                              , Group "att" $ map Var [ "grammatical_number", "GrammaticalNumber", "_", adj ] -- TODO: Is this right?
                              ]) -- TODO: Verify that this only happens on position 1
                      ]
                in results
              _ -> fixme "not-np"
          Just fun ->
            fixme fun
          _ ->
            fixme "unknown"

-- Relevant word attributes: case (others?)
generateVerb :: Text -> [ImmutableArray StoSyntax.Feat] -> IO ()
generateVerb kind args = do
  pure ()
-- Verb syntactic functions:
-- - adverbialComplement
-- - directObject
-- - formalSubject
-- - indirectObject
-- - objectComplement
-- - prepositionalComplement
-- - somPrepComplement
-- - subject

generateFrames :: ImmutableArray StoSyntax.SubcategorizationFrame -> IO ()
generateFrames frames = do
  printCode
    "phrase_att"
    [ Var "case", Var "unspecified", Var "PhraseGroup"]
    [ Group "phraseGroup" $ map Var [ "_", "PhraseGroup" ] ]
  printCode
    "phrase_att"
    [ Var "definiteness", Var "Definiteness", Var "PhraseGroup"]
    [ Group "phraseGroup" $ map Var [ "Definiteness", "PhraseGroup" ] ]
  mapM_ handleFrame frames
  where handleFrame :: StoSyntax.SubcategorizationFrame -> IO ()
        handleFrame (StoSyntax.SubcategorizationFrame attrs _feats lexemeProperty syntacticArguments) =
                     let frame = fixFrame $ fromJust $ StoSyntax.subcategorizationFrameId attrs
                         partOfSpeech = do
                           StoSyntax.LexemeProperty lexemePropertyFeats <- lexemeProperty
                           getFeat' lexemePropertyFeats StoSyntax.Feat_att_partOfSpeech
                         argumentGroups = map (map snd)
                                          $ unflattenArguments
                                          $ groupArguments
                                          $ map makeArgument
                                          $ ArrI.elems syntacticArguments
                     in case partOfSpeech of
                       Nothing -> pure () -- "not yet analysed" according to the XML, ignore
                       Just "noun" -> mapM_ (generateNoun frame) argumentGroups
                       Just "adjective" -> mapM_ (generateAdjective frame) argumentGroups
                       Just "verb" -> mapM_ (generateVerb frame) argumentGroups
                       _ -> error "unexpected part of speech"
                     -- in do
                     --   T.putStrLn frame
                     --   print partOfSpeech
                     --   mapM_ (\group -> mapM_ print group >> T.putStrLn "") argumentGroups
                     --   T.putStrLn ""

        makeArgument :: StoSyntax.SyntacticArgument -> ArgumentWithIndex
        makeArgument (StoSyntax.SyntacticArgument _attrs feats) =
          (read $ T.unpack $ getFeat feats StoSyntax.Feat_att_positionNumber,
           feats)

        groupArguments :: [ArgumentWithIndex] -> [[ArgumentWithIndex]]
        groupArguments = \case
          args@((i, _) : _) ->
            let (group, rest) = L.span (\(j, _) -> i == j) args
            in group : groupArguments rest
          [] -> []

        unflattenArguments :: [[ArgumentWithIndex]] -> [[ArgumentWithIndex]]
        unflattenArguments = \case
          group : groups -> do
            arg <- group
            map (arg :) $ unflattenArguments groups
          [] -> [[]]
