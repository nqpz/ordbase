module Main (main) where

import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import Data.Store (decodeEx)
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.Lzma as Lzma
import qualified Data.Array.IArray as ArrI

import qualified StoMorphology
import StoFiles (morphXmlPaths)
import FileEmbedding (morphsToLexiconString)

testFilename :: FilePath
testFilename = "morphs.lzma"

-- Example: Parse an XML file and re-format it to standard out.
compress :: IO ()
compress = do
  raw <- morphsToLexiconString =<< morphXmlPaths
  let compressed = Lzma.compress $ BS.fromStrict raw
  BS.writeFile testFilename compressed

decompress :: IO ()
decompress = do
  compressed <- BS.readFile testFilename
  let raw = decodeEx $ BS.toStrict compressed
  let xml = StoMorphology.Lexicon (ArrI.listArray (1, 0) []) raw
  hPutXml stdout False xml
  hFlush stdout

main :: IO ()
main = do
  compress
  decompress
