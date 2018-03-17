module ChordAnalysis where
import Notes
import Scales
import Chords

----------------------
-- Chord progressions
----------------------

modalChordProgression Ionian = [majorChord, minorChord, minorChord, majorChord, majorChord, minorChord, dimChord]
modalChordProgression mode = shiftWith modalChordProgression mode

chordProgression (Modal mode startNote) = map (\(i, chordType) -> (chordType.scaleInterval i.Modal mode) startNote) (zip [1..7] (modalChordProgression mode))

-------------------
-- Chord Functions
-------------------

nthChord n scale = (chordProgression scale) !! (n-1)

applied nth ofN scale =
  let newScale =  scale { startNote = tonic (nthChord ofN scale) }
  in nthChord nth newScale

borrow nth otherMode scale =
  let newScale = scale { mode = otherMode }
  in nthChord nth newScale

--------------------
-- Chord analysis!
--------------------

allPossibleFunctions = nthFunctions ++ appliedFunctions ++ borrowedFunctions
  where
    nths = zip ["I", "II", "III", "IV", "V", "VI", "VII"] [1..7]
    nthFunctions = [ (numeral, nthChord nth) | (numeral, nth) <- nths ]
    appliedFunctions = [ (numeral ++ "/" ++ ofNumeral, applied nth ofN) | (numeral, nth) <- nths, (ofNumeral, ofN) <- nths ]
    borrowedFunctions = [ (numeral ++ " " ++ show mode, borrow nth mode) | (numeral, nth) <- nths, mode <- [Ionian .. Locrian] ]

analyzeChord chord scale = (map fst . filter ((== chord) . ($ scale) . snd)) allPossibleFunctions