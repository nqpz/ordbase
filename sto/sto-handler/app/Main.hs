module Main (main) where

import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import Data.Store (decodeEx)
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.Lzma as Lzma
import qualified Data.Array.IArray as ArrI

import Types
import qualified StoMorphology
import qualified StoSyntax
import StoFiles (morphXmlPaths, syntaxXmlPaths)
import FileEmbedding (morphsToString, syntaxsToStrings)

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
  raw <- morphsToString paths
  putStrLn "compress"
  let compressed = Lzma.compress $ BS.fromStrict raw
  putStrLn "write"
  BS.writeFile morphsTestFilename compressed

morphsDecompressAndUnstoreAndPrint :: IO ()
morphsDecompressAndUnstoreAndPrint = do
  putStrLn "read"
  compressed <- BS.readFile morphsTestFilename
  putStrLn "decompress"
  let raw = Lzma.decompress compressed
  putStrLn "build xml"
  let lexicalEntries = decodeEx $ BS.toStrict raw
      xml = StoMorphology.Lexicon emptyArray lexicalEntries
  putStrLn "write xml"
  hPutXml stdout False xml
  hFlush stdout

syntaxTest :: IO ()
syntaxTest = do
  paths <- syntaxXmlPaths
  mapM_ putStrLn paths
  (lexsRaw, framesRaw) <- syntaxsToStrings paths
  let (lexsCompressed, framesCompressed) = (Lzma.compress $ BS.fromStrict lexsRaw,
                                            Lzma.compress $ BS.fromStrict framesRaw)
      (lexsDecompressed, framesDecompressed) = (Lzma.decompress lexsCompressed,
                                                Lzma.decompress framesCompressed)
      (lexs, frames) = (decodeEx $ BS.toStrict lexsDecompressed,
                        decodeEx $ BS.toStrict framesDecompressed)
      xml = StoSyntax.Lexicon emptyArray lexs frames
  hPutXml stdout False xml
  hFlush stdout

main :: IO ()
main = do
  morphsStoreAndCompress
  morphsDecompressAndUnstoreAndPrint
  syntaxTest
