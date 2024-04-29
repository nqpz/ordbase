module Main (main) where

import System.Environment (getArgs)
import System.IO (stdout, hFlush)
import Text.XML.HaXml.XmlContent (hPutXml)
import Data.Store (decodeEx)
import qualified Data.Array.IArray as ArrI

import qualified StoMorphology
import FileEmbedding (storeFiles)
-- import MorphEmbed (morphs)

-- Example: Parse an XML file and re-format it to standard out.
main :: IO ()
main = do
  morphologyXmlFilePaths <- getArgs
  contents <- storeFiles morphologyXmlFilePaths
  let xml = StoMorphology.Lexicon (ArrI.listArray (1, 0) []) (decodeEx contents)
  hPutXml stdout False xml
  hFlush stdout
