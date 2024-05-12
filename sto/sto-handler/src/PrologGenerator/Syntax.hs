module PrologGenerator.Syntax
  ( generateProlog
  ) where

import Types
import PrologGenerator
import qualified StoSyntax

generateProlog :: ImmutableArray StoSyntax.LexicalEntry
               -> ImmutableArray StoSyntax.SubcategorizationFrame
               -> IO ()
generateProlog entries frames = do
  generateEntries entries
  generateFrames frames

generateEntries :: ImmutableArray StoSyntax.LexicalEntry -> IO ()
generateEntries entries = undefined

generateFrames :: ImmutableArray StoSyntax.SubcategorizationFrame -> IO ()
generateFrames frames = undefined
