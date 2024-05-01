{-# LANGUAGE TemplateHaskell #-}
module EmbeddedData
  ( morphologyLexicalEntries
  , syntaxLexicalEntries
  , syntaxSubcategorizationFrames
  ) where

import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import Data.Store (decodeEx)
import qualified Codec.Compression.Lzma as Lzma

import StoFiles (morphXmlPaths, syntaxXmlPaths)
import FileEmbedding (embedMorphs, embedSyntaxs)
import Types
import qualified StoMorphology
import qualified StoSyntax

decompress :: ByteString -> ByteString
decompress = BSL.toStrict . Lzma.decompress . BSL.fromStrict

morphologyLexicalEntriesString :: ByteString
morphologyLexicalEntriesString = $(embedMorphs morphXmlPaths)

morphologyLexicalEntries :: ImmutableArray StoMorphology.LexicalEntry
morphologyLexicalEntries = decodeEx $ decompress $ morphologyLexicalEntriesString

syntaxLexicalEntriesAndFramesString :: (ByteString, ByteString)
syntaxLexicalEntriesAndFramesString = $(embedSyntaxs syntaxXmlPaths)

syntaxLexicalEntries :: ImmutableArray StoSyntax.LexicalEntry
syntaxLexicalEntries = decodeEx $ decompress $ fst syntaxLexicalEntriesAndFramesString

syntaxSubcategorizationFrames :: ImmutableArray StoSyntax.SubcategorizationFrame
syntaxSubcategorizationFrames = decodeEx $ decompress $ snd syntaxLexicalEntriesAndFramesString
