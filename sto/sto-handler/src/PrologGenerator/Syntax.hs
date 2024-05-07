module PrologGenerator.Syntax
  ( generateProlog
  ) where

import Types
import PrologGenerator
import qualified StoSyntax

generateProlog :: ImmutableArray StoSyntax.LexicalEntry
               -> ImmutableArray StoSyntax.SubcategorizationFrame
               -> IO ()
generateProlog entries frames = undefined
