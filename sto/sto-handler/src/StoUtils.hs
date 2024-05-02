{-# LANGUAGE LambdaCase #-}
module StoUtils
  ( defaultableText
  , defaultableString
  ) where

import Text.XML.HaXml.XmlContent
import Data.Text (Text)
import qualified Data.Text as T

defaultableText :: Defaultable String -> Defaultable Text
defaultableText = \case
  Default s -> Default $ T.pack s
  NonDefault s -> NonDefault $ T.pack s

defaultableString :: Defaultable Text -> Defaultable String
defaultableString = \case
  Default t -> Default $ T.unpack t
  NonDefault t -> NonDefault $ T.unpack t
