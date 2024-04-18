module Extsubset where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types
import Text.XML.HaXml.OneOfN


{-Type decls-}

data LexicalResource = LexicalResource LexicalResource_Attrs [Feat]
                                       GlobalInformation (List1 Lexicon)
                     deriving (Eq,Show)
data LexicalResource_Attrs = LexicalResource_Attrs
    { lexicalResourceDtdVersion :: (Defaultable String)
    , lexicalResourceXmlns'dcr :: (Defaultable String)
    } deriving (Eq,Show)
newtype GlobalInformation = GlobalInformation [Feat] 		deriving (Eq,Show)
data Lexicon = Lexicon [Feat] (List1 LexicalEntry)
             deriving (Eq,Show)
data LexicalEntry = LexicalEntry LexicalEntry_Attrs [Feat] Lemma
                                 [WordForm] [RelatedForm]
                  deriving (Eq,Show)
data LexicalEntry_Attrs = LexicalEntry_Attrs
    { lexicalEntryId :: (Maybe String)
    } deriving (Eq,Show)
data Lemma = Lemma [Feat] [FormRepresentation]
           deriving (Eq,Show)
data WordForm = WordForm [Feat] [FormRepresentation]
              deriving (Eq,Show)
newtype FormRepresentation = FormRepresentation [Feat] 		deriving (Eq,Show)
data RelatedForm = RelatedForm RelatedForm_Attrs [Feat]
                               [FormRepresentation]
                 deriving (Eq,Show)
data RelatedForm_Attrs = RelatedForm_Attrs
    { relatedFormTargets :: (Maybe String)
    } deriving (Eq,Show)
data Feat = Feat
    { featAtt :: Feat_att
    , featVal :: String
    , featDcr'valueDatcat :: (Maybe String)
    , featDcr'datcat :: (Maybe String)
    } deriving (Eq,Show)
data Feat_att = Feat_att_id  |  Feat_att_morphologicalUnitId  | 
                Feat_att_partOfSpeech  |  Feat_att_originalSource  | 
                Feat_att_independentWord  |  Feat_att_officiallyApproved  | 
                Feat_att_frequency  |  Feat_att_languageCoding  | 
                Feat_att_languageIdentifier  |  Feat_att_adjectivalFunction  | 
                Feat_att_degree  |  Feat_att_grammaticalGender  | 
                Feat_att_grammaticalNumber  |  Feat_att_definiteness  | 
                Feat_att_transcategorization  |  Feat_att_case  | 
                Feat_att_joiningElement  |  Feat_att_joiningElementResult  | 
                Feat_att_decomposition  |  Feat_att_ownerNumber  |  Feat_att_person
                 |  Feat_att_reflexivity  |  Feat_att_register  | 
                Feat_att_verbFormMood  |  Feat_att_tense  |  Feat_att_voice  | 
                Feat_att_writtenForm  |  Feat_att_inflectionalParadigm  | 
                Feat_att_spellingVariant
              deriving (Eq,Show)


{-Instance decls-}

instance HTypeable LexicalResource where
    toHType x = Defined "LexicalResource" [] []
instance XmlContent LexicalResource where
    toContents (LexicalResource as a b c) =
        [CElem (Elem (N "LexicalResource") (toAttrs as) (concatMap toContents a
                                                         ++ toContents b ++ toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["LexicalResource"]
        ; interior e $ return (LexicalResource (fromAttrs as))
                       `apply` many parseContents `apply` parseContents
                       `apply` parseContents
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
    toHType x = Defined "GlobalInformation" [] []
instance XmlContent GlobalInformation where
    toContents (GlobalInformation a) =
        [CElem (Elem (N "GlobalInformation") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["GlobalInformation"]
        ; interior e $ return (GlobalInformation)
                       `apply` many parseContents
        } `adjustErr` ("in <GlobalInformation>, "++)

instance HTypeable Lexicon where
    toHType x = Defined "Lexicon" [] []
instance XmlContent Lexicon where
    toContents (Lexicon a b) =
        [CElem (Elem (N "Lexicon") [] (concatMap toContents a ++
                                       toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Lexicon"]
        ; interior e $ return (Lexicon) `apply` many parseContents
                       `apply` parseContents
        } `adjustErr` ("in <Lexicon>, "++)

instance HTypeable LexicalEntry where
    toHType x = Defined "LexicalEntry" [] []
instance XmlContent LexicalEntry where
    toContents (LexicalEntry as a b c d) =
        [CElem (Elem (N "LexicalEntry") (toAttrs as) (concatMap toContents a
                                                      ++ toContents b ++ concatMap toContents c ++
                                                      concatMap toContents d)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["LexicalEntry"]
        ; interior e $ return (LexicalEntry (fromAttrs as))
                       `apply` many parseContents `apply` parseContents
                       `apply` many parseContents `apply` many parseContents
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
    toHType x = Defined "Lemma" [] []
instance XmlContent Lemma where
    toContents (Lemma a b) =
        [CElem (Elem (N "Lemma") [] (concatMap toContents a ++
                                     concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["Lemma"]
        ; interior e $ return (Lemma) `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <Lemma>, "++)

instance HTypeable WordForm where
    toHType x = Defined "WordForm" [] []
instance XmlContent WordForm where
    toContents (WordForm a b) =
        [CElem (Elem (N "WordForm") [] (concatMap toContents a ++
                                        concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["WordForm"]
        ; interior e $ return (WordForm) `apply` many parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <WordForm>, "++)

instance HTypeable FormRepresentation where
    toHType x = Defined "FormRepresentation" [] []
instance XmlContent FormRepresentation where
    toContents (FormRepresentation a) =
        [CElem (Elem (N "FormRepresentation") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["FormRepresentation"]
        ; interior e $ return (FormRepresentation)
                       `apply` many parseContents
        } `adjustErr` ("in <FormRepresentation>, "++)

instance HTypeable RelatedForm where
    toHType x = Defined "RelatedForm" [] []
instance XmlContent RelatedForm where
    toContents (RelatedForm as a b) =
        [CElem (Elem (N "RelatedForm") (toAttrs as) (concatMap toContents a
                                                     ++ concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["RelatedForm"]
        ; interior e $ return (RelatedForm (fromAttrs as))
                       `apply` many parseContents `apply` many parseContents
        } `adjustErr` ("in <RelatedForm>, "++)
instance XmlAttributes RelatedForm_Attrs where
    fromAttrs as =
        RelatedForm_Attrs
          { relatedFormTargets = possibleA fromAttrToStr "targets" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "targets" (relatedFormTargets v)
        ]

instance HTypeable Feat where
    toHType x = Defined "feat" [] []
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
      where translate "id" = Just Feat_att_id
            translate "morphologicalUnitId" = Just Feat_att_morphologicalUnitId
            translate "partOfSpeech" = Just Feat_att_partOfSpeech
            translate "originalSource" = Just Feat_att_originalSource
            translate "independentWord" = Just Feat_att_independentWord
            translate "officiallyApproved" = Just Feat_att_officiallyApproved
            translate "frequency" = Just Feat_att_frequency
            translate "languageCoding" = Just Feat_att_languageCoding
            translate "languageIdentifier" = Just Feat_att_languageIdentifier
            translate "adjectivalFunction" = Just Feat_att_adjectivalFunction
            translate "degree" = Just Feat_att_degree
            translate "grammaticalGender" = Just Feat_att_grammaticalGender
            translate "grammaticalNumber" = Just Feat_att_grammaticalNumber
            translate "definiteness" = Just Feat_att_definiteness
            translate "transcategorization" = Just Feat_att_transcategorization
            translate "case" = Just Feat_att_case
            translate "joiningElement" = Just Feat_att_joiningElement
            translate "joiningElementResult" = Just Feat_att_joiningElementResult
            translate "decomposition" = Just Feat_att_decomposition
            translate "ownerNumber" = Just Feat_att_ownerNumber
            translate "person" = Just Feat_att_person
            translate "reflexivity" = Just Feat_att_reflexivity
            translate "register" = Just Feat_att_register
            translate "verbFormMood" = Just Feat_att_verbFormMood
            translate "tense" = Just Feat_att_tense
            translate "voice" = Just Feat_att_voice
            translate "writtenForm" = Just Feat_att_writtenForm
            translate "inflectionalParadigm" = Just Feat_att_inflectionalParadigm
            translate "spellingVariant" = Just Feat_att_spellingVariant
            translate _ = Nothing
    toAttrFrTyp n Feat_att_id = Just (N n, str2attr "id")
    toAttrFrTyp n Feat_att_morphologicalUnitId = Just (N n, str2attr "morphologicalUnitId")
    toAttrFrTyp n Feat_att_partOfSpeech = Just (N n, str2attr "partOfSpeech")
    toAttrFrTyp n Feat_att_originalSource = Just (N n, str2attr "originalSource")
    toAttrFrTyp n Feat_att_independentWord = Just (N n, str2attr "independentWord")
    toAttrFrTyp n Feat_att_officiallyApproved = Just (N n, str2attr "officiallyApproved")
    toAttrFrTyp n Feat_att_frequency = Just (N n, str2attr "frequency")
    toAttrFrTyp n Feat_att_languageCoding = Just (N n, str2attr "languageCoding")
    toAttrFrTyp n Feat_att_languageIdentifier = Just (N n, str2attr "languageIdentifier")
    toAttrFrTyp n Feat_att_adjectivalFunction = Just (N n, str2attr "adjectivalFunction")
    toAttrFrTyp n Feat_att_degree = Just (N n, str2attr "degree")
    toAttrFrTyp n Feat_att_grammaticalGender = Just (N n, str2attr "grammaticalGender")
    toAttrFrTyp n Feat_att_grammaticalNumber = Just (N n, str2attr "grammaticalNumber")
    toAttrFrTyp n Feat_att_definiteness = Just (N n, str2attr "definiteness")
    toAttrFrTyp n Feat_att_transcategorization = Just (N n, str2attr "transcategorization")
    toAttrFrTyp n Feat_att_case = Just (N n, str2attr "case")
    toAttrFrTyp n Feat_att_joiningElement = Just (N n, str2attr "joiningElement")
    toAttrFrTyp n Feat_att_joiningElementResult = Just (N n, str2attr "joiningElementResult")
    toAttrFrTyp n Feat_att_decomposition = Just (N n, str2attr "decomposition")
    toAttrFrTyp n Feat_att_ownerNumber = Just (N n, str2attr "ownerNumber")
    toAttrFrTyp n Feat_att_person = Just (N n, str2attr "person")
    toAttrFrTyp n Feat_att_reflexivity = Just (N n, str2attr "reflexivity")
    toAttrFrTyp n Feat_att_register = Just (N n, str2attr "register")
    toAttrFrTyp n Feat_att_verbFormMood = Just (N n, str2attr "verbFormMood")
    toAttrFrTyp n Feat_att_tense = Just (N n, str2attr "tense")
    toAttrFrTyp n Feat_att_voice = Just (N n, str2attr "voice")
    toAttrFrTyp n Feat_att_writtenForm = Just (N n, str2attr "writtenForm")
    toAttrFrTyp n Feat_att_inflectionalParadigm = Just (N n, str2attr "inflectionalParadigm")
    toAttrFrTyp n Feat_att_spellingVariant = Just (N n, str2attr "spellingVariant")



{-Done-}
