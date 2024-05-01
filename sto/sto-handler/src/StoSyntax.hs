{-# LANGUAGE TemplateHaskell #-}

module StoSyntax
  ( LexicalResource(..)
  , LexicalResource_Attrs(..)
  , GlobalInformation(..)
  , Lexicon(..)
  , LexicalEntry(..)
  , LexicalEntry_Attrs(..)
  , Lemma(..)
  , FormRepresentation(..)
  , SyntacticBehaviour(..)
  , SyntacticBehaviour_Attrs(..)
  , SubcategorizationFrame(..)
  , SubcategorizationFrame_Attrs(..)
  , LexemeProperty(..)
  , SyntacticArgument(..)
  , SyntacticArgument_Attrs(..)
  , Feat(..)
  , Feat_att(..)
  , extractLexicalEntries
  , extractSubcategorizationFrames
  ) where

import Text.XML.HaXml.XmlContent hiding (many)
import Text.XML.HaXml.Types (QName(..))
import TH.Derive
import Data.Store

import Types
import ArrayUtils

extractLexicalEntries :: LexicalResource -> ImmutableArray LexicalEntry
extractLexicalEntries (LexicalResource _ _ _ lexicons) =
  ensureSingleton (fmap (\(Lexicon _ entries _) -> entries) lexicons)

extractSubcategorizationFrames :: LexicalResource -> ImmutableArray SubcategorizationFrame
extractSubcategorizationFrames (LexicalResource _ _ _ lexicons) =
  ensureSingleton (fmap (\(Lexicon _ _ frames) -> frames) lexicons)


{-Type decls-}

data LexicalResource = LexicalResource LexicalResource_Attrs (ImmutableArray Feat)
                                       GlobalInformation (ImmutableArray1 Lexicon)
  deriving (Eq, Show)

data LexicalResource_Attrs = LexicalResource_Attrs { lexicalResourceDtdVersion :: Defaultable String
                                                   , lexicalResourceXmlns'dcr :: Defaultable String
                                                   }
  deriving (Eq, Show)

newtype GlobalInformation = GlobalInformation (ImmutableArray Feat)
  deriving (Eq, Show)

data Lexicon = Lexicon (ImmutableArray Feat) (ImmutableArray1 LexicalEntry)
                       (ImmutableArray SubcategorizationFrame)
  deriving (Eq, Show)

data LexicalEntry = LexicalEntry LexicalEntry_Attrs (ImmutableArray Feat) Lemma
                                 (ImmutableArray SyntacticBehaviour)
  deriving (Eq, Show)

data LexicalEntry_Attrs = LexicalEntry_Attrs { lexicalEntryId :: Maybe String
                                             }
  deriving (Eq, Show)

data Lemma = Lemma (ImmutableArray Feat) (ImmutableArray FormRepresentation)
  deriving (Eq, Show)

newtype FormRepresentation = FormRepresentation (ImmutableArray Feat)
  deriving (Eq, Show)

data SyntacticBehaviour = SyntacticBehaviour SyntacticBehaviour_Attrs
                                             (ImmutableArray Feat)
  deriving (Eq, Show)

data SyntacticBehaviour_Attrs = SyntacticBehaviour_Attrs { syntacticBehaviourId :: Maybe String
                                                         , syntacticBehaviourSubcategorizationFrames :: Maybe String
                                                         }
  deriving (Eq, Show)

data SubcategorizationFrame = SubcategorizationFrame SubcategorizationFrame_Attrs
                                                     (ImmutableArray Feat) (Maybe LexemeProperty)
                                                     (ImmutableArray SyntacticArgument)
  deriving (Eq, Show)

data SubcategorizationFrame_Attrs = SubcategorizationFrame_Attrs { subcategorizationFrameId :: Maybe String
                                                                 }
  deriving (Eq, Show)

newtype LexemeProperty = LexemeProperty (ImmutableArray Feat)
  deriving (Eq, Show)

data SyntacticArgument = SyntacticArgument SyntacticArgument_Attrs
                                           (ImmutableArray Feat)
  deriving (Eq, Show)

data SyntacticArgument_Attrs = SyntacticArgument_Attrs { syntacticArgumentId :: Maybe String
                                                       }
  deriving (Eq, Show)

data Feat = Feat { featAtt :: Feat_att
                 , featVal :: String
                 , featDcr'valueDatcat :: Maybe String
                 , featDcr'datcat :: Maybe String
                 }
  deriving (Eq, Show)

data Feat_att = Feat_att_languageCoding
              | Feat_att_languageIdentifier
              | Feat_att_id
              | Feat_att_morphologicalUnitId
              | Feat_att_writtenForm
              | Feat_att_officiallyApproved
              | Feat_att_synuId
              | Feat_att_example
              | Feat_att_naming
              | Feat_att_constructionId
              | Feat_att_partOfSpeech
              | Feat_att_selfId
              | Feat_att_reflexiveVerb
              | Feat_att_takesParticle
              | Feat_att_takesAuxiliary
              | Feat_att_passiveVerb
              | Feat_att_modal
              | Feat_att_auxiliary
              | Feat_att_positionNumber
              | Feat_att_syntacticFunctionType
              | Feat_att_optional
              | Feat_att_syntacticConstituentLabel
              | Feat_att_syntacticConstituentPhraseId
              | Feat_att_case
              | Feat_att_reflexiveVoice
              | Feat_att_expletive
              | Feat_att_definiteness
              | Feat_att_npIndex
              | Feat_att_introducer
              | Feat_att_ppComplementLabel
              | Feat_att_controlType
              | Feat_att_coreferenceRelation
              | Feat_att_clauseType
              | Feat_att_finite
              | Feat_att_adjectivalFunction
  deriving (Eq, Show)

$($(derive [d|instance Deriving (Store Feat_att)|]))
$($(derive [d|instance Deriving (Store Feat)|]))
$($(derive [d|instance Deriving (Store FormRepresentation)|]))
$($(derive [d|instance Deriving (Store SyntacticBehaviour_Attrs)|]))
$($(derive [d|instance Deriving (Store SyntacticBehaviour)|]))
$($(derive [d|instance Deriving (Store SyntacticArgument_Attrs)|]))
$($(derive [d|instance Deriving (Store SyntacticArgument)|]))
$($(derive [d|instance Deriving (Store LexemeProperty)|]))
$($(derive [d|instance Deriving (Store SubcategorizationFrame_Attrs)|]))
$($(derive [d|instance Deriving (Store SubcategorizationFrame)|]))
$($(derive [d|instance Deriving (Store Lemma)|]))
$($(derive [d|instance Deriving (Store LexicalEntry_Attrs)|]))
$($(derive [d|instance Deriving (Store LexicalEntry)|]))


{-Instance decls-}

instance HTypeable LexicalResource where
    toHType _x = Defined "LexicalResource" [] []
instance XmlContent LexicalResource where
    toContents (LexicalResource as a b c) =
        [CElem (Elem (N "LexicalResource") (toAttrs as) (concatMap toContents a
                                                         ++ toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["LexicalResource"]
        ; interior e $ return (LexicalResource (fromAttrs as))
                       `apply` many parseContents `apply` parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <LexicalResource>, "++)
instance XmlAttributes LexicalResource_Attrs where
    fromAttrs as =
        LexicalResource_Attrs
          { lexicalResourceDtdVersion = defaultA fromAttrToStr "16" "dtdVersion" as
          , lexicalResourceXmlns'dcr = defaultA fromAttrToStr "http://www.isocat.org/ns/dcr" "xmlns:dcr" as
          }
    toAttrs v = catMaybes
        [ defaultToAttr toAttrFrStr "dtdVersion" (lexicalResourceDtdVersion v)
        , defaultToAttr toAttrFrStr "xmlns:dcr" (lexicalResourceXmlns'dcr v)
        ]

instance HTypeable GlobalInformation where
    toHType _x = Defined "GlobalInformation" [] []
instance XmlContent GlobalInformation where
    toContents (GlobalInformation a) =
        [CElem (Elem (N "GlobalInformation") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["GlobalInformation"]
        ; interior e $ return (GlobalInformation)
                       `apply` many parseContents
        } `adjustErr` ("in <GlobalInformation>, "++)

instance HTypeable Lexicon where
    toHType _x = Defined "Lexicon" [] []
instance XmlContent Lexicon where
    toContents (Lexicon a b c) =
        [CElem (Elem (N "Lexicon") [] (concatMap toContents a ++
                                       concatMap toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Lexicon"]
        ; interior e $ return (Lexicon) `apply` many parseContents
                       `apply` many parseContents `apply` many parseContents
        } `adjustErr` ("in <Lexicon>, "++)

instance HTypeable LexicalEntry where
    toHType _x = Defined "LexicalEntry" [] []
instance XmlContent LexicalEntry where
    toContents (LexicalEntry as a b c) =
        [CElem (Elem (N "LexicalEntry") (toAttrs as) (concatMap toContents a
                                                      ++ toContents b ++
                                                      concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["LexicalEntry"]
        ; interior e $ return (LexicalEntry (fromAttrs as))
                       `apply` many parseContents `apply` parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <LexicalEntry>, "++)
instance XmlAttributes LexicalEntry_Attrs where
    fromAttrs as =
        LexicalEntry_Attrs
          { lexicalEntryId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes
        [ maybeToAttr toAttrFrStr "id" (lexicalEntryId v)
        ]

instance HTypeable Lemma where
    toHType _x = Defined "Lemma" [] []
instance XmlContent Lemma where
    toContents (Lemma a b) =
        [CElem (Elem (N "Lemma") [] (concatMap toContents a ++
                                     concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Lemma"]
        ; interior e $ return (Lemma) `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <Lemma>, "++)

instance HTypeable FormRepresentation where
    toHType _x = Defined "FormRepresentation" [] []
instance XmlContent FormRepresentation where
    toContents (FormRepresentation a) =
        [CElem (Elem (N "FormRepresentation") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["FormRepresentation"]
        ; interior e $ return (FormRepresentation)
                       `apply` many parseContents
        } `adjustErr` ("in <FormRepresentation>, "++)

instance HTypeable SyntacticBehaviour where
    toHType _x = Defined "SyntacticBehaviour" [] []
instance XmlContent SyntacticBehaviour where
    toContents (SyntacticBehaviour as a) =
        [CElem (Elem (N "SyntacticBehaviour") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["SyntacticBehaviour"]
        ; interior e $ return (SyntacticBehaviour (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <SyntacticBehaviour>, "++)
instance XmlAttributes SyntacticBehaviour_Attrs where
    fromAttrs as =
        SyntacticBehaviour_Attrs
          { syntacticBehaviourId = possibleA fromAttrToStr "id" as
          , syntacticBehaviourSubcategorizationFrames = possibleA fromAttrToStr "subcategorizationFrames" as
          }
    toAttrs v = catMaybes
        [ maybeToAttr toAttrFrStr "id" (syntacticBehaviourId v)
        , maybeToAttr toAttrFrStr "subcategorizationFrames" (syntacticBehaviourSubcategorizationFrames v)
        ]

instance HTypeable SubcategorizationFrame where
    toHType _x = Defined "SubcategorizationFrame" [] []
instance XmlContent SubcategorizationFrame where
    toContents (SubcategorizationFrame as a b c) =
        [CElem (Elem (N "SubcategorizationFrame") (toAttrs as) (concatMap toContents a
                                                                ++ maybe [] toContents b ++
                                                                concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["SubcategorizationFrame"]
        ; interior e $ return (SubcategorizationFrame (fromAttrs as))
                       `apply` many parseContents `apply` optional parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <SubcategorizationFrame>, "++)
instance XmlAttributes SubcategorizationFrame_Attrs where
    fromAttrs as =
        SubcategorizationFrame_Attrs
          { subcategorizationFrameId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes
        [ maybeToAttr toAttrFrStr "id" (subcategorizationFrameId v)
        ]

instance HTypeable LexemeProperty where
    toHType _x = Defined "LexemeProperty" [] []
instance XmlContent LexemeProperty where
    toContents (LexemeProperty a) =
        [CElem (Elem (N "LexemeProperty") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["LexemeProperty"]
        ; interior e $ return (LexemeProperty) `apply` many parseContents
        } `adjustErr` ("in <LexemeProperty>, "++)

instance HTypeable SyntacticArgument where
    toHType _x = Defined "SyntacticArgument" [] []
instance XmlContent SyntacticArgument where
    toContents (SyntacticArgument as a) =
        [CElem (Elem (N "SyntacticArgument") (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["SyntacticArgument"]
        ; interior e $ return (SyntacticArgument (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <SyntacticArgument>, "++)
instance XmlAttributes SyntacticArgument_Attrs where
    fromAttrs as =
        SyntacticArgument_Attrs
          { syntacticArgumentId = possibleA fromAttrToStr "id" as
          }
    toAttrs v = catMaybes
        [ maybeToAttr toAttrFrStr "id" (syntacticArgumentId v)
        ]

instance HTypeable Feat where
    toHType _x = Defined "feat" [] []
instance XmlContent Feat where
    toContents as =
        [CElem (Elem (N "feat") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["feat"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <feat>, "++)
instance XmlAttributes Feat where
    fromAttrs as =
        Feat
          { featAtt = definiteA fromAttrToTyp "feat" "att" as
          , featVal = definiteA fromAttrToStr "feat" "val" as
          , featDcr'valueDatcat = possibleA fromAttrToStr "dcr:valueDatcat" as
          , featDcr'datcat = possibleA fromAttrToStr "dcr:datcat" as
          }
    toAttrs v = catMaybes
        [ toAttrFrTyp "att" (featAtt v)
        , toAttrFrStr "val" (featVal v)
        , maybeToAttr toAttrFrStr "dcr:valueDatcat" (featDcr'valueDatcat v)
        , maybeToAttr toAttrFrStr "dcr:datcat" (featDcr'datcat v)
        ]

instance XmlAttrType Feat_att where
    fromAttrToTyp n (N n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "languageCoding" = Just Feat_att_languageCoding
            translate "languageIdentifier" = Just Feat_att_languageIdentifier
            translate "id" = Just Feat_att_id
            translate "morphologicalUnitId" = Just Feat_att_morphologicalUnitId
            translate "writtenForm" = Just Feat_att_writtenForm
            translate "officiallyApproved" = Just Feat_att_officiallyApproved
            translate "synuId" = Just Feat_att_synuId
            translate "example" = Just Feat_att_example
            translate "naming" = Just Feat_att_naming
            translate "constructionId" = Just Feat_att_constructionId
            translate "partOfSpeech" = Just Feat_att_partOfSpeech
            translate "selfId" = Just Feat_att_selfId
            translate "reflexiveVerb" = Just Feat_att_reflexiveVerb
            translate "takesParticle" = Just Feat_att_takesParticle
            translate "takesAuxiliary" = Just Feat_att_takesAuxiliary
            translate "passiveVerb" = Just Feat_att_passiveVerb
            translate "modal" = Just Feat_att_modal
            translate "auxiliary" = Just Feat_att_auxiliary
            translate "positionNumber" = Just Feat_att_positionNumber
            translate "syntacticFunctionType" = Just Feat_att_syntacticFunctionType
            translate "optional" = Just Feat_att_optional
            translate "syntacticConstituentLabel" = Just Feat_att_syntacticConstituentLabel
            translate "syntacticConstituentPhraseId" = Just Feat_att_syntacticConstituentPhraseId
            translate "case" = Just Feat_att_case
            translate "reflexiveVoice" = Just Feat_att_reflexiveVoice
            translate "expletive" = Just Feat_att_expletive
            translate "definiteness" = Just Feat_att_definiteness
            translate "npIndex" = Just Feat_att_npIndex
            translate "introducer" = Just Feat_att_introducer
            translate "ppComplementLabel" = Just Feat_att_ppComplementLabel
            translate "controlType" = Just Feat_att_controlType
            translate "coreferenceRelation" = Just Feat_att_coreferenceRelation
            translate "clauseType" = Just Feat_att_clauseType
            translate "finite" = Just Feat_att_finite
            translate "adjectivalFunction" = Just Feat_att_adjectivalFunction
            translate _ = Nothing
    fromAttrToTyp _ _ = Nothing
    toAttrFrTyp n Feat_att_languageCoding = Just (N n, str2attr "languageCoding")
    toAttrFrTyp n Feat_att_languageIdentifier = Just (N n, str2attr "languageIdentifier")
    toAttrFrTyp n Feat_att_id = Just (N n, str2attr "id")
    toAttrFrTyp n Feat_att_morphologicalUnitId = Just (N n, str2attr "morphologicalUnitId")
    toAttrFrTyp n Feat_att_writtenForm = Just (N n, str2attr "writtenForm")
    toAttrFrTyp n Feat_att_officiallyApproved = Just (N n, str2attr "officiallyApproved")
    toAttrFrTyp n Feat_att_synuId = Just (N n, str2attr "synuId")
    toAttrFrTyp n Feat_att_example = Just (N n, str2attr "example")
    toAttrFrTyp n Feat_att_naming = Just (N n, str2attr "naming")
    toAttrFrTyp n Feat_att_constructionId = Just (N n, str2attr "constructionId")
    toAttrFrTyp n Feat_att_partOfSpeech = Just (N n, str2attr "partOfSpeech")
    toAttrFrTyp n Feat_att_selfId = Just (N n, str2attr "selfId")
    toAttrFrTyp n Feat_att_reflexiveVerb = Just (N n, str2attr "reflexiveVerb")
    toAttrFrTyp n Feat_att_takesParticle = Just (N n, str2attr "takesParticle")
    toAttrFrTyp n Feat_att_takesAuxiliary = Just (N n, str2attr "takesAuxiliary")
    toAttrFrTyp n Feat_att_passiveVerb = Just (N n, str2attr "passiveVerb")
    toAttrFrTyp n Feat_att_modal = Just (N n, str2attr "modal")
    toAttrFrTyp n Feat_att_auxiliary = Just (N n, str2attr "auxiliary")
    toAttrFrTyp n Feat_att_positionNumber = Just (N n, str2attr "positionNumber")
    toAttrFrTyp n Feat_att_syntacticFunctionType = Just (N n, str2attr "syntacticFunctionType")
    toAttrFrTyp n Feat_att_optional = Just (N n, str2attr "optional")
    toAttrFrTyp n Feat_att_syntacticConstituentLabel = Just (N n, str2attr "syntacticConstituentLabel")
    toAttrFrTyp n Feat_att_syntacticConstituentPhraseId = Just (N n, str2attr "syntacticConstituentPhraseId")
    toAttrFrTyp n Feat_att_case = Just (N n, str2attr "case")
    toAttrFrTyp n Feat_att_reflexiveVoice = Just (N n, str2attr "reflexiveVoice")
    toAttrFrTyp n Feat_att_expletive = Just (N n, str2attr "expletive")
    toAttrFrTyp n Feat_att_definiteness = Just (N n, str2attr "definiteness")
    toAttrFrTyp n Feat_att_npIndex = Just (N n, str2attr "npIndex")
    toAttrFrTyp n Feat_att_introducer = Just (N n, str2attr "introducer")
    toAttrFrTyp n Feat_att_ppComplementLabel = Just (N n, str2attr "ppComplementLabel")
    toAttrFrTyp n Feat_att_controlType = Just (N n, str2attr "controlType")
    toAttrFrTyp n Feat_att_coreferenceRelation = Just (N n, str2attr "coreferenceRelation")
    toAttrFrTyp n Feat_att_clauseType = Just (N n, str2attr "clauseType")
    toAttrFrTyp n Feat_att_finite = Just (N n, str2attr "finite")
    toAttrFrTyp n Feat_att_adjectivalFunction = Just (N n, str2attr "adjectivalFunction")



{-Done-}
