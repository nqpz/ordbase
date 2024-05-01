{-# LANGUAGE TemplateHaskell #-}
module EmbeddedData
  ( morphologyLexicalEntries
  , syntaxLexicalEntries
  , syntaxSubcategorizationFrames
  ) where

import Data.ByteString (ByteString)
import Data.Store (decodeEx)

import StoFiles (morphXmlPaths, syntaxXmlPaths)
import FileEmbedding (embedMorphs, embedSyntaxs)
import Types
import qualified StoMorphology
import qualified StoSyntax

morphologyLexicalEntriesString :: ByteString
morphologyLexicalEntriesString = $(embedMorphs morphXmlPaths)

morphologyLexicalEntries :: ImmutableArray StoMorphology.LexicalEntry
morphologyLexicalEntries = decodeEx morphologyLexicalEntriesString

syntaxLexicalEntriesAndFramesString :: (ByteString, ByteString)
syntaxLexicalEntriesAndFramesString = $(embedSyntaxs syntaxXmlPaths)

syntaxLexicalEntries :: ImmutableArray StoSyntax.LexicalEntry
syntaxLexicalEntries = decodeEx $ fst syntaxLexicalEntriesAndFramesString

syntaxSubcategorizationFrames :: ImmutableArray StoSyntax.SubcategorizationFrame
syntaxSubcategorizationFrames = decodeEx $ snd syntaxLexicalEntriesAndFramesString
