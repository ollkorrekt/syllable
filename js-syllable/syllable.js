// syllable.js
// This module defines the syllable structure and random generation utilities.

// Expanded nuclei types (based on Haskell Vowel, OVowel, IVowel, etc.)
const NUCLEI = [
  // El nuclei (L type)
  { type: 'El', value: 'æl' },
  { type: 'El', value: 'ɑl' },
  { type: 'El', value: 'ɛl' },
  { type: 'El', value: 'əl' },
  { type: 'El', value: 'ɪl' },
  // ElDi (diphthongal l)
  { type: 'El', value: 'ejl' },
  { type: 'El', value: 'ərl' },
  { type: 'El', value: 'owl' },
  { type: 'El', value: 'ijl' },
  { type: 'El', value: 'uwl' },
  // Other nuclei (Vowel, OVowel, IVowel, etc.)
  { type: 'Vowel', value: 'æ' },
  { type: 'Vowel', value: 'ɑ' },
  { type: 'Vowel', value: 'ɛ' },
  { type: 'Vowel', value: 'ə' },
  { type: 'Vowel', value: 'ɪ' },
  { type: 'Vowel', value: 'ʊ' },
  { type: 'Vowel', value: 'ej' },
  { type: 'Vowel', value: 'ər' },
  { type: 'Vowel', value: 'ow' },
  { type: 'Vowel', value: 'ij' },
  { type: 'Vowel', value: 'uw' },
  { type: 'Vowel', value: 'æw' },
  { type: 'Vowel', value: 'ɑr' },
  { type: 'Vowel', value: 'ɑj' },
  { type: 'Vowel', value: 'er' },
  { type: 'Vowel', value: 'əj' },
  { type: 'Vowel', value: 'or' },
  { type: 'Vowel', value: 'oj' },
  { type: 'Vowel', value: 'ir' },
  { type: 'Vowel', value: 'ur' },
  // OVowel
  { type: 'OVowel', value: 'oj' },
  { type: 'OVowel', value: 'or' },
  // IVowel
  { type: 'IVowel', value: 'ij' },
  { type: 'IVowel', value: 'ir' }
];

// Expanded onsets (based on Haskell Onset)
const ONSETS = [
  'h', '', 'w', 'l', 'r', 'hj', 'j', 'p', 'b', 'pw', 'bw', 'pl', 'bl', 'pr', 'br', 'pj', 'bj',
  'f', 'v', 'fw', 'vw', 'fl', 'vl', 'fr', 'vr', 'fj', 'vj', 'm', 'mw', 'mj', 'θ', 'ð', 'θw', 'θr', 'θj',
  't', 'd', 'tw', 'dw', 'tr', 'dr', 'n', 'tʃ', 'dʒ', 'tʃw', 'dʒw', 'k', 'ɡ', 'kw', 'ɡw', 'kl', 'ɡl',
  'kr', 'ɡr', 'kj', 'ɡj', 's', 'z', 'sw', 'zw', 'sl', 'sp', 'spw', 'spl', 'spr', 'spj', 'sf', 'sfw',
  'sfl', 'sfr', 'sfj', 'sm', 'smw', 'smj', 'st', 'stw', 'str', 'sn', 'sk', 'skw', 'skl', 'skr', 'skj',
  'ʃ', 'ʒ', 'ʃw', 'ʒw', 'ʃl', 'ʃr', 'ʃp', 'ʃpw', 'ʃpl', 'ʃpr', 'ʃpj', 'ʃf', 'ʃfw', 'ʃfl', 'ʃfr', 'ʃfj',
  'ʃm', 'ʃmw', 'ʃmj', 'ʃt', 'ʃtw', 'ʃn', 'ʃk', 'ʃkw', 'ʃkl', 'ʃkr', 'ʃkj'
];


// Define coda sets for each rime type (based on Haskell logic)
const NASAL_CODAS = ['m', 'n'];
const NONASAL_CODAS = ['b', 'v', 'ɡ'];
const FRICSTOP_CODAS = [
  'p', 'pθ', 'pt', 'ps', 'f', 'ft', 'θ', 'ð', 't', 'd', 's', 'z',
  'sp', 'st', 'sk', 'ʃ', 'ʒ'
];
const AFFRIC_CODAS = ['tθ', 'ts', 'tʃ', 'dʒ'];
const OBSTRUENT_CODAS = [...AFFRIC_CODAS, ...FRICSTOP_CODAS];
const ENGMA_CODAS = ['', 'k', 'kθ', 'kt', 'ks'];

// Helper: which nuclei are ShortNucleus (Vowel or ElMono)
function isShortNucleus(nucleus) {
  // In Haskell: ShortNucleus = SNV Vowel | SNL ElMono
  // We'll treat all Vowel and ElMono as short, ElDi as not
  return nucleus.type === 'Vowel' || (nucleus.type === 'El' && !nucleus.value.endsWith('w'));
}

// Helper: which nuclei are ElDi (El diphthongs)
function isElDi(nucleus) {
  // In Haskell: ElDi = Lejl | Lərl | Lowl | Lijl | Luwl
  return nucleus.type === 'El' && nucleus.value.endsWith('l');
}

// Helper: which nuclei are Engma (special set)
const ENGMA_NUCLEI = [
  { type: 'Engma', value: 'ajŋ' },
  { type: 'Engma', value: 'aŋ' },
  { type: 'Engma', value: 'ejŋ' },
  { type: 'Engma', value: 'əŋ' },
  { type: 'Engma', value: 'ojŋ' },
  { type: 'Engma', value: 'ijŋ' }
];

// Helper: which nuclei are Nucleus (Vowel, ElMono, ElDi)
function isNucleus(nucleus) {
  return nucleus.type === 'Vowel' || nucleus.type === 'El';
}

// Generate only valid syllables according to rime logic
const SYLLABLES = [];
for (let onset of ONSETS) {
  for (let nucleus of NUCLEI) {
    // RNasal: ShortNucleus + NasalCoda
    if (isShortNucleus(nucleus)) {
      for (let coda of NASAL_CODAS) {
        SYLLABLES.push({ onset, nucleus, coda });
      }
    }
    // RNoNasal: Nucleus + NoNasalCoda
    if (isNucleus(nucleus)) {
      for (let coda of NONASAL_CODAS) {
        SYLLABLES.push({ onset, nucleus, coda });
      }
    }
    // RObstruent: ShortNucleus + FricativeStopCoda
    if (isShortNucleus(nucleus)) {
      for (let coda of FRICSTOP_CODAS) {
        SYLLABLES.push({ onset, nucleus, coda });
      }
    }
    // RAffricative: ShortNucleus + AffricativeCoda
    if (isShortNucleus(nucleus)) {
      for (let coda of AFFRIC_CODAS) {
        SYLLABLES.push({ onset, nucleus, coda });
      }
    }
    // RElDiObstruent: ElDi + ObstruentCoda
    if (isElDi(nucleus)) {
      for (let coda of OBSTRUENT_CODAS) {
        SYLLABLES.push({ onset, nucleus, coda });
      }
    }
    // RNoEngma: Nucleus + EngmaCoda
    if (isNucleus(nucleus)) {
      for (let coda of ENGMA_CODAS) {
        SYLLABLES.push({ onset, nucleus, coda });
      }
    }
  }
  // REngmaCoda: Engma + EngmaCoda
  for (let nucleus of ENGMA_NUCLEI) {
    for (let coda of ENGMA_CODAS) {
      SYLLABLES.push({ onset, nucleus, coda });
    }
  }
}

// Returns a random element from an array
function randomChoice(arr) {
  return arr[Math.floor(Math.random() * arr.length)];
}

// Simulate isL property (true if nucleus type is 'El')
function isL(nucleus) {
  return nucleus.type === 'El';
}

// Generate a random nucleus
function randomNucleus() {
  return randomChoice(NUCLEI);
}

// Generate a random syllable
function randomSyllable() {
  return randomChoice(SYLLABLES);
}

export { NUCLEI, SYLLABLES, isL, randomNucleus, randomSyllable };
