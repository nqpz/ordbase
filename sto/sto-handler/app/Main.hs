module Main (main) where

import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import Data.Store (decodeEx)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Array.IArray as ArrI
import qualified Codec.Compression.Lzma as Lzma

import Types
import qualified StoMorphology
import qualified StoSyntax
import StoFiles (morphXmlPaths, syntaxXmlPaths)
import FileEmbedding (morphsToString, syntaxsToStrings)
import qualified EmbeddedData

emptyArray :: ImmutableArray e
emptyArray = ArrI.listArray (1, 0) []

morphsTestFilename :: FilePath
morphsTestFilename = "morphs.lzma"

-- Example: Parse an XML file and re-format it to standard out.
morphsStoreAndCompress :: IO ()
morphsStoreAndCompress = do
  putStrLn "get paths"
  paths <- morphXmlPaths
  mapM_ putStrLn paths
  putStrLn "make bytes"
  compressed <- morphsToString paths
  putStrLn "write"
  BSL.writeFile morphsTestFilename $ BSL.fromStrict compressed

morphsDecompressAndUnstoreAndPrint :: IO ()
morphsDecompressAndUnstoreAndPrint = do
  putStrLn "read"
  compressed <- BSL.readFile morphsTestFilename
  putStrLn "build xml"
  let lexicalEntries = decodeEx $ BSL.toStrict $ Lzma.decompress compressed
      xml = StoMorphology.Lexicon emptyArray lexicalEntries
  putStrLn "write xml"
  hPutXml stdout False xml
  hFlush stdout

syntaxTest :: IO ()
syntaxTest = do
  paths <- syntaxXmlPaths
  mapM_ putStrLn paths
  (lexsCompressed, framesCompressed) <- syntaxsToStrings paths
  let (lexsDecompressed, framesDecompressed) = (BSL.toStrict $ Lzma.decompress $ BSL.fromStrict lexsCompressed,
                                                BSL.toStrict $ Lzma.decompress $ BSL.fromStrict framesCompressed)
      (lexs, frames) = (decodeEx lexsDecompressed,
                        decodeEx framesDecompressed)
      xml = StoSyntax.Lexicon emptyArray lexs frames
  hPutXml stdout False xml
  hFlush stdout

testBasics :: IO ()
testBasics = do
  morphsStoreAndCompress
  morphsDecompressAndUnstoreAndPrint
  syntaxTest

testEmbeddedData :: IO ()
testEmbeddedData = do
  hPutXml stdout False $ StoMorphology.Lexicon emptyArray EmbeddedData.morphologyLexicalEntries
  hFlush stdout

  hPutXml stdout False $ StoSyntax.Lexicon emptyArray (EmbeddedData.syntaxLexicalEntries) (EmbeddedData.syntaxSubcategorizationFrames)
  hFlush stdout

main :: IO ()
main = do
  putStrLn "basics"
  testBasics
  putStrLn "embedded data"
  testEmbeddedData
