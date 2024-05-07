{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module PrologGenerator
  ( escape
  , fixId
  , camelCaseToSnakeCase
  , fact
  , Exp(..)
  , printExp
  , printCode
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (forM_)
import Data.Char (toLower, isUpper, isNumber)

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

fact :: Text -> [Text] -> IO ()
fact name ps = printCode name (map Var ps) []

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
