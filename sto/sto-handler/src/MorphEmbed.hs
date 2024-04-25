{-# LANGUAGE TemplateHaskell #-}

module MorphEmbed where

import qualified StoMorphology
import FileEmbedding (embedFiles)

morphAB :: [StoMorphology.LexicalEntry]
morphAB = $(embedFiles
             [ "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_a_jan2013.xml"
             , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_b_jan2013.xml"
             ])
