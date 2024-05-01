module Main (main) where

import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import Data.Store (decodeEx)
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.Lzma as Lzma
import qualified Data.Array.IArray as ArrI

import qualified StoMorphology
import StoFiles (morphXmlPaths)
import FileEmbedding (morphsToString)

testFilename :: FilePath
testFilename = "morphs.lzma"

-- Example: Parse an XML file and re-format it to standard out.
storeAndCompress :: IO ()
storeAndCompress = do
  putStrLn "get paths"
  paths <- morphXmlPaths
  mapM_ putStrLn paths
  putStrLn "make bytes"
  raw <- morphsToString paths
  putStrLn "compress"
  let compressed = Lzma.compress $ BS.fromStrict raw
  putStrLn "write"
  BS.writeFile testFilename compressed

decompressAndUnstoreAndPrint :: IO ()
decompressAndUnstoreAndPrint = do
  putStrLn "read"
  compressed <- BS.readFile testFilename
  putStrLn "decompress"
  let raw = Lzma.decompress compressed
  putStrLn "build xml"
  let lexicalEntries = decodeEx $ BS.toStrict raw
      xml = StoMorphology.Lexicon (ArrI.listArray (1, 0) []) lexicalEntries
  putStrLn "write xml"
  hPutXml stdout False xml
  hFlush stdout

main :: IO ()
main = do
  storeAndCompress
  decompressAndUnstoreAndPrint
