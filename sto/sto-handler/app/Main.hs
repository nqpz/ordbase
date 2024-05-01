module Main (main) where

import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import Data.Store (decodeEx)
import qualified Data.Array.IArray as ArrI
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.Lzma as Lzma

import qualified StoMorphology
import FileEmbedding (toLexiconString)
-- import MorphEmbed (morphs)

-- Example: Parse an XML file and re-format it to standard out.
main :: IO ()
main = do
  morphologyXmlFilePaths <- getArgs
  contents <- toLexiconString morphologyXmlFilePaths
  BS.writeFile "morphs.lzma" (Lzma.compress (BS.fromStrict contents))
  -- contents <- BS.readFile "morphxmls.store"
  -- let xml = StoMorphology.Lexicon (ArrI.listArray (1, 0) []) (decodeEx contents)
  -- hPutXml stdout False xml
  -- hFlush stdout
