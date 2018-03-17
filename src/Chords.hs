{-# LANGUAGE OverloadedStrings #-}

module Chords where
import Data.List
import Notes
import Data.Aeson
import Scales

data Chord = Ch {chordName :: String, numberedNotes :: [(Int, Note)]}
data ChordAlteration = ChAlt {nameAlt::String, alterationEffect::([(Int,Note)] -> [(Int, Note)])}

instance WithNotes Chord where
  notes = map snd.sortOn fst.numberedNotes

instance Eq Chord where
--  (==) chord1 chord2 = notes chord1 == notes chord2
-- At least for now allow matching notes with the same sound (but not exactly equal)
  (==) chord1 = all (\(n1, n2) -> sameSound n1 n2).zip (notes chord1).notes

alterChord alteration (Ch name numberedNotes) = Ch (name ++ show alteration) ((alterationEffect alteration) numberedNotes)

-------------------------
-- Common chord creation
-------------------------

majorChord note = Ch (show note) (zip [1,3,5] baseChordNotes)
  where baseChordNotes = (map ($ note) [id, major 3, (\baseNote -> perfectFifth (sharpsOrFlats (Modal Ionian baseNote)) baseNote)])

minorChord = alterChord (ChAlt "-" lowerThird).majorChord

augChord = alterChord (ChAlt "+" (semitoneUp 5)).majorChord

dimChord = alterChord (ChAlt "°" (lowerThird.lowerFifth)).majorChord

susChord n note | n == 2 || n == 4 = alteredChord
  where alteredChord = alterChord (ChAlt ("sus"++show n)(replace 3 (\_ -> major n))) (majorChord note)

dim7Chord = alterChord (ChAlt "7" (add 6 major)).dimChord
halfDim7Chord = alterChord flatFive.chord7.minorChord

chord7 = alterChord seventh
chordMaj7 = alterChord majorSeventh

--------------------------------
-- Additional chord alterations
-- Use with alterChord
--------------------------------

flatFive = ChAlt "(b5)" lowerFifth
sixth = ChAlt "6" (add 6 major)
seventh = ChAlt "7" (add 7 minor)
majorSeventh = ChAlt "maj7" (add 7 major)

overtone n = ChAlt ("("++show n++")") (add n major)
augOvertone n = ChAlt ("(#"++show n++")") (semitoneUp n . add n major)

----------------------------
-- Common alteration combos
----------------------------

upToOvertone n startChord = (foldl (flip alterChord) (chord7 startChord).map overtone.filter odd) [9 .. n]

-----------------------------
-- Base alteration functions
-----------------------------

major n  = scaleInterval n . Modal Ionian
minor n  = scaleInterval n . Modal Aeolian
lowerThird = replace 3 minor
lowerFifth = replace 5 (\n -> scaleInterval n.Modal Locrian)

-- Is it ok to make this always major?
semitoneUp n = replace n (\m -> semitone up Sharp . major m)

replace n intervalFunction numberedNotes = add n intervalFunction (filter ((/= n).fst) numberedNotes)

add n intervalFunction numberedNotes = (n, (intervalFunction n) firstNote) : numberedNotes
  where firstNote = (snd.head.filter ((==1).fst)) numberedNotes

allChords note = map ($ note) [majorChord, minorChord, augChord, dimChord, dim7Chord]

---- Pritty print

instance Show Chord where
  show = chordName
  
instance Show ChordAlteration where
  show (ChAlt name _) = name

instance ToJSON Chord where
  toJSON (Ch name numberedNotes) = object ["name" .= name, "numberedNotes" .= numberedNotes]