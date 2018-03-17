{-# LANGUAGE OverloadedStrings #-}

module Scales where
import Notes
import Data.Aeson

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian deriving (Eq, Show, Enum, Bounded)
data Scale = Modal {mode :: Mode, startNote :: Note}

allModes :: [Mode]
allModes = [minBound .. maxBound]

instance WithNotes Scale where
  notes (Modal mode startNote) = (init . foldNotesFrom startNote . scale alteration) mode
    where alteration = sharpsOrFlats (Modal mode startNote)

sharpsOrFlats (Modal mode startNote)
  | (elem startNote . drop 6 . circleOfFifths . naturalNote) mode = Flat
  | otherwise = Sharp

scale preferredAlteration = map (\f -> f up preferredAlteration).scalePattern

scalePattern Ionian = tone : tone : semitone : tone : tone : tone : semitone : []
scalePattern mode = shiftWith scalePattern mode

naturalNote mode = N ([C .. B] !! (fromEnum mode)) Nat

scaleInterval :: Int -> Scale -> Note
scaleInterval n scale = notes scale !! mod (n-1) 7

chromatics preferredAlteration = iterate (semitone up preferredAlteration)

circleOfFifths = flip foldNotesFrom ((take 6 (fifthsWith Sharp)) ++ (take 6 (drop 6 (fifthsWith Flat))))
  where
    fifthsWith preferredAlteration = (repeat.perfectFifth) preferredAlteration

perfectFifth preferredAlteration = head.drop 7.chromatics preferredAlteration

----  AUX
shiftWith f = shiftFirst.f.pred
shiftFirst (x:xs) = xs ++ [x]

foldNotesFrom startNote = foldl (\notes f -> notes ++ [(f.last) notes]) [startNote]

instance ToJSON Mode where
  toJSON mode = object [ "mode" .= show mode ]