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
  -- generateEntries entries
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

generateFrames :: ImmutableArray StoSyntax.SubcategorizationFrame -> IO ()
generateFrames = mapM_ handleFrame
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
                       Just "noun" -> pure () -- todo
                       Just "adjective" -> pure () -- todo
                       Just "verb" -> pure () -- todo
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
