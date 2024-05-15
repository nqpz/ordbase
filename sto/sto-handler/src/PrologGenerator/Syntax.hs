{-# LANGUAGE OverloadedStrings #-}
module PrologGenerator.Syntax
  ( generateProlog
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromJust)
import Data.Foldable (foldl')
import Control.Applicative ((<|>))
import Control.Monad (when, guard, forM_)

import Types
import PrologGenerator
import qualified StoSyntax

generateProlog :: ImmutableArray StoSyntax.LexicalEntry
               -> ImmutableArray StoSyntax.SubcategorizationFrame
               -> IO ()
generateProlog entries frames = do
  generateEntries entries
  generateFrames frames

fixFrame :: Text -> Text
fixFrame frame = T.concat [ "kind_", T.replace "-" "_" frame]

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

        getFeat feats = fromJust . getFeat' feats

        getFeat' :: ImmutableArray StoSyntax.Feat
                 -> StoSyntax.Feat_att -> Maybe Text
        getFeat' feats att = foldl' (\prev feat -> prev <|> do
                                        guard $ StoSyntax.featAtt feat == att
                                        pure $ StoSyntax.featVal feat)
                             Nothing feats

generateFrames :: ImmutableArray StoSyntax.SubcategorizationFrame -> IO ()
generateFrames frames = pure ()
