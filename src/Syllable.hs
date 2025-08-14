{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Syllable (Syllable, Onset, Nucleus, isL) where
import System.Random ()
import System.Random.Stateful (uniformEnumM, uniformRM, Uniform(..), Finite)
import GHC.Generics

--TODO iw at start of word or after ʔ

data Onset
    = Oh | O_ | Ow | Ol | Or | Ohj | Oj | Op | Ob | Opw | Obw | Opl | Obl | Opr
    | Obr | Opj | Obj | Of | Ov | Ofw | Ovw | Ofl | Ovl | Ofr | Ovr | Ofj | Ovj
    | Om | Omw | Omj | Oθ | Oð | Oθw | Oθr | Oθj | Ot | Od | Otw | Odw | Otr
    | Odr | On | Otʃ | Odʒ | Otʃw | Odʒw | Ok | Og | Okw | Ogw | Okl | Ogl | Okr
    | Ogr | Okj | Ogj | Os | Oz | Osw | Ozw | Osl | Osp | Ospw | Ospl | Ospr
    | Ospj | Osf | Osfw | Osfl | Osfr | Osfj | Osm | Osmw | Osmj | Ost | Ostw
    | Ostr | Osn | Osk | Oskw | Oskl | Oskr | Oskj | Oʃ | Oʒ | Oʃw | Oʒw | Oʃl
    | Oʃr | Oʃp | Oʃpw | Oʃpl | Oʃpr | Oʃpj | Oʃf | Oʃfw | Oʃfl | Oʃfr | Oʃfj
    | Oʃm | Oʃmw | Oʃmj | Oʃt | Oʃtw | Oʃn | Oʃk | Oʃkw | Oʃkl | Oʃkr | Oʃkj
  deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform Onset
instance Show Onset where
    show x = reprs !! fromEnum x where 
        reprs = --TODO remove j, w, perhaps r onsets 
            [ "h", "", "w", "l", "r", "hj", "j", "p", "b", "pw", "bw", "pl"
            , "bl", "pr", "br", "pj", "bj", "f", "v", "fw", "vw", "fl", "vl"
            , "fr", "vr", "fj", "vj", "m", "mw", "mj", "θ", "ð", "θw", "θr"
            , "θj", "t", "d", "tw", "dw", "tr", "dr", "n", "tʃ", "dʒ", "tʃw"
            , "dʒw", "k", "\x261", "kw", "\x261w", "kl", "\x261l", "kr"
            , "\x261r", "kj", "\x261j", "s", "z", "sw", "zw", "sl", "sp", "spw"
            , "spl", "spr", "spj", "sf", "sfw", "sfl", "sfr", "sfj", "sm", "smw"
            , "smj", "st", "stw", "str", "sn", "sk", "skw", "skl", "skr", "skj"
            , "ʃ", "ʒ", "ʃw", "ʒw", "ʃl", "ʃr", "ʃp", "ʃpw", "ʃpl", "ʃpr", "ʃpj"
            , "ʃf", "ʃfw", "ʃfl", "ʃfr", "ʃfj", "ʃm", "ʃmw", "ʃmj", "ʃt", "ʃtw"
            , "ʃn", "ʃk", "ʃkw", "ʃkl", "ʃkr", "ʃkj"]

--data JOnset = Ohj | Opj | Obj | Ofj | Ovj | Omj | Oθj
--data WOnset = Opw | Obw | Ofw | Ovw | Omw | Oθw | Otw | Odw
--data ROnset = Or ???? | Opr | Obr | O
--data LOnset = Ol ???? |

data Vowel --TODO remove oj or from here, fix OVowel and IVowel; ijŋ and ojŋ are
--problematic.
    = Væ | Va | Ve | Və | Vi | Vu | Vej | Vər | Vow | Vij | Vuw | Vaw | Var
    | Vaj | Ver | Vəj | Vor | Voj | Vir | Vur
  deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform Vowel
instance Show Vowel where
    show x = reprs !! fromEnum x where
        reprs =
            [ "æ", "\x251", "ɛ", "ə", "\x26a", "ʊ", "ej", "ər", "ow", "ij", "uw"
            , "æw", "\x251r", "\x251j", "er", "əj", "or", "oj", "ir", "ur" ]

data OVowel = VOj | VOr --cannot be precded by /Cw/ exept for alveolar consonant
  deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform OVowel
instance Show OVowel where
    show x = reprs !! fromEnum x where
        reprs = ["oj",  "or"]

data IVowel = VIj | VIr -- cannot be preceded by /Cj/
  deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform IVowel
instance Show IVowel where
    show x = reprs !! fromEnum x where
        reprs = ["ij",  "ir"]

data ElMono = Læl | Lal | Lel | Ləl | Lil
    deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform ElMono
instance Show ElMono where
    show x = reprs !! fromEnum x where
        reprs = ["æl", "\x251l", "ɛl", "əl", "\x26al"]

data ElDi = Lejl | Lərl | Lowl | Lijl | Luwl
    deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Show ElDi where
    show x = reprs !! fromEnum x where
        reprs = ["ejl", "ərl", "owl", "ijl", "uwl"]
            
data Engma = Gajŋ | Gaŋ | Gejŋ | Gəŋ | Gojŋ | Gijŋ --vowels plus engma, which has a restricted distribution
    deriving (Bounded, Enum, Eq, Finite, Generic, Ord) --TODO separate ijŋ, ojŋ

instance Uniform Engma
instance Show Engma where
    show x = reprs !! fromEnum x where
        reprs = ["\x251jŋ", "\x251ŋ", "ejŋ", "əŋ", "ojŋ", "ijŋ"]

data NasalCoda = Nm | Nn --codas with just a nasal alone (but not the restricted engma); there are 2 instead of just 1 possible nasal like for other codas
    deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform NasalCoda
instance Show NasalCoda where
    show x = reprs !! fromEnum x where
        reprs = ["m", "n"]

data NoNasalCoda = NNb | NNv | NNg --codas with an obstruent that may not be preceded by a homorganic nasal
    deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform NoNasalCoda
instance Show NoNasalCoda where
    show x = reprs !! fromEnum x where
        reprs = ["b", "v", "\x261"]

data AffricativeCoda --not distinguished from fricative codas after nasals. (except marginally for ns - nts; not in my dialect)
    = Btθ | Bts | Btʃ | Bdʒ
  deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform AffricativeCoda
instance Show AffricativeCoda where
    show x = reprs !! fromEnum x where
        reprs = ["tθ", "ts", "tʃ", "dʒ"]

data FricativeStopCoda --the codas which a homorganic nasal may precede (or not)
    = Bp | Bpθ | Bpt | Bps | Bf | Bft | Bθ | Bð | Bt | Bd | Bs | Bz | Bsp | Bst
    | Bsk | Bʃ | Bʒ
  deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform FricativeStopCoda
instance Show FricativeStopCoda where
    show x = reprs !! fromEnum x where
        reprs =
            [ "p", "pθ", "pt", "ps", "f", "ft", "θ", "ð", "t", "d", "s", "z"
            , "sp", "st", "sk", "ʃ", "ʒ" ]

homorganic :: FricativeStopCoda -> String
homorganic x = reprs !! fromEnum x where
    reprs =
        [ "m", "m", "m", "m", "m", "m", "n", "n", "n", "n", "n", "n", "n", "n"
        , "n", "nt", "nd" ]

data ObstruentCoda = BA AffricativeCoda | BFS FricativeStopCoda
    deriving (Eq, Finite, Generic)
instance Uniform ObstruentCoda
instance Show ObstruentCoda where
    showsPrec n (BA x) = showsPrec n x
    showsPrec n (BFS x) = showsPrec n x

-- | Codas that can follow Engma, or a vowel directly. treated seperately from F.S.Coda because of restricted distribution of engma. empty coda covered here.
data EngmaCoda = B_ | Bk | Bkθ | Bkt | Bks
    deriving (Bounded, Enum, Eq, Finite, Generic, Ord)
instance Uniform EngmaCoda
instance Show EngmaCoda where
    show x = reprs !! fromEnum x where
        reprs = ["", "k", "kθ", "kt", "ks"]

data Nucleus = NV Vowel | NLM ElMono | NLD ElDi deriving (Eq, Finite, Generic)
instance Uniform Nucleus
instance Show Nucleus where
    show (NV x) = show x
    show (NLM x) = show x
    show (NLD x) = show x

data ShortNucleus = SNV Vowel | SNL ElMono deriving (Eq, Finite, Generic)
instance Uniform ShortNucleus
instance Show ShortNucleus where
    show (SNV x) = show x
    show (SNL x) = show x

isL :: Nucleus -> Bool
isL (NV _) = False
isL _ = True


data Rime
    = RNasal ShortNucleus NasalCoda
        -- ^ Finals that end in /n/ or /m/ by themselves.
    | RNoNasal Nucleus NoNasalCoda
        -- ^ Finals that end in a coda incompatible with nasals.
    | RObstruent -- ^ Most finals that contain a non-velar obstruent in the coda.
        ShortNucleus -- ^ The nucleus & following approximants
        FricativeStopCoda -- ^ one or two non-velar obstruents
    | RNasalObstruent -- ^ Finals that contain a nasal followed by a homorganic non-velar obstruent.
        Vowel -- ^ The nucleus & following approximants
        FricativeStopCoda -- ^ one or two non-velar obstruents
    | RAffricative -- ^ Finals that contain either an affricate or a stop followed by a homorganic fricative.
        ShortNucleus
        AffricativeCoda
    | RElDiObstruent -- ^ Finals that contain a diphthong followed by /l/ and an obstruent
        ElDi
        ObstruentCoda
    | RNoEngma Nucleus EngmaCoda 
        -- ^ Finals that happen to have an EngmaCoda, but no engma.
    | REngmaCoda Engma EngmaCoda -- ^ Finals that contain engma.
  deriving (Eq, Finite, Generic)
instance Uniform Rime
instance Show Rime where
    showsPrec _ (RNasal nuc coda) = shows nuc . shows coda
    showsPrec _ (RNoNasal nuc coda) = shows nuc . shows coda
    showsPrec _ (RObstruent nuc coda) = shows nuc . shows coda
    showsPrec _ (RNasalObstruent nuc coda)
        = shows nuc . (homorganic coda ++) . shows coda
    showsPrec _ (RAffricative nuc coda) = shows nuc . shows coda
    showsPrec _ (RElDiObstruent nuc coda) =  shows nuc . shows coda
    showsPrec _ (RNoEngma nuc coda) = shows nuc . shows coda
    showsPrec _ (REngmaCoda eng coda) = shows eng . shows coda

data Syllable = Syllable Onset Rime deriving (Eq, Finite, Generic) --wor and woj (wow is uncommon but fine in quo, quote, quota, swollen, swole); jir and jij are marginal after consonants; also rər; but swore, thwart, jocular sword, (~)dwarf, ~Dworkin, ~quarter, ~quorum, ~quart, ~quart-, ~quartz, swarf, swarm, swarthy, shwarma; ~Iroquois, ~sequoia, ~turquoise; ?br'er no examples for -ji-.
instance Uniform Syllable
instance Show Syllable where
    show (Syllable ons fin) = shows ons $ shows fin ""
