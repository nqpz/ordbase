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
        clean ',' = "_" -- probably safe
        clean '-' = "_dash_"
        clean '\'' = "_apostrophe_"
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

data Exp = Group Text [Exp]
         | Var Text

printExp :: Exp -> IO ()
printExp = \case
  Group name args -> do
    T.putStr name
    case args of
      arg0 : args' -> do
        T.putStr "("
        printExp arg0
        forM_ args' $ \arg -> do
          T.putStr ", "
          printExp arg
        T.putStr ")"
      [] -> pure ()
  Var v ->
    T.putStr v

printCode :: Text -> [Exp] -> [Exp] -> IO ()
printCode name args subs = do
  printExp (Group name args)
  case subs of
    sub0 : subs' -> do
      T.putStrLn " :-"
      printExp sub0
      forM_ subs' $ \sub -> do
        T.putStrLn ","
        printExp sub
    [] ->
      pure ()
  T.putStrLn "."
  T.putStrLn ""

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
