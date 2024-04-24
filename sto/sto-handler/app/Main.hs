module Main (main) where

-- import System.Environment (getArgs)
import System.IO (stdout, hFlush)
-- import Text.XML.HaXml.XmlContent (fReadXml, hPutXml)
import Text.XML.HaXml.XmlContent (hPutXml)

-- import qualified StoMorphology
import MorphEmbed

-- Example: Parse an XML file and re-format it to standard out.
main :: IO ()
main = do
  -- args <- getArgs
  -- contents <-
  --   case args of
  --     [morphologyXmlFilePath] -> fReadXml morphologyXmlFilePath :: IO StoMorphology.LexicalResource
  --     _ -> error "missing an argument"
  hPutXml stdout False morphA -- contents
  hFlush stdout
