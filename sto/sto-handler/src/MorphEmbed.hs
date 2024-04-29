{-# LANGUAGE TemplateHaskell #-}

module MorphEmbed (
  -- morphs
  ) where

import Data.ByteString
import qualified StoMorphology
-- import FileEmbedding (embedFiles)

-- morphs :: ByteString
-- morphs = $(embedFiles
--              [ -- "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_a_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_b_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_c_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_d_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_e_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_f_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_g_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_h_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_i_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_j_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_k_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_l_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_m_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_n_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_o_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_p_jan2013.xml"
--              -- ,
--              "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_q_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_r_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_s_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_t_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_u_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_v_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_xyz_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_noun_rest_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_adj_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_extract_a_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_pronoun_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_verb_jan2013.xml"
--              -- , "../data/STO_morphology_v2_-_LMF_format/STO-LEXICON-MORPHOLOGY-v2-LMF/STO_LMF_morphology_rest_jan2013.xml"
--              ])
