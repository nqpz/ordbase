{-# LANGUAGE TemplateHaskell #-}

module MorphEmbed where

import qualified StoMorphology
import FileEmbedding (embedFile)

morphA :: [StoMorphology.LexicalEntry]
morphA = $(embedFile "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_a_jan2013.xml")
