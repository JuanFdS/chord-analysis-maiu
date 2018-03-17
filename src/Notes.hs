{-# LANGUAGE OverloadedStrings #-}

module Notes where
import Data.List
import Data.Maybe
import Data.Aeson
import Web.Scotty (Parsable, parseParam)
import Data.Text.Lazy (unpack)
import qualified Data.Text as T (unpack)

data BasicNote = C | D | E | F | G | A | B deriving (Enum, Show, Eq, Read)
data NoteAlteration = Flat | Nat | Sharp deriving (Eq, Read)
data Note = N {basicNote::BasicNote, alt::NoteAlteration} deriving Eq

-- instance Read NoteAlteration where
--   readsPrec _ string = map (\note -> (note , "")) (filter ((== string).show) allNotes)

allNotes = [N basicNote alt | basicNote <- [C ..], alt <- [Nat, Flat, Sharp]]

noteToNumber (N C Nat) = 1
noteToNumber (N B Nat) = 0
noteToNumber (N F Nat) = noteToNumber (N E Sharp)
noteToNumber (N B Flat) = noteToNumber (N A Sharp)
noteToNumber (N basicNote Nat) = noteToNumber (N (pred basicNote) Nat) + 2
noteToNumber (N basicNote Flat) = noteToNumber (N basicNote Nat) - 1
noteToNumber (N basicNote Sharp) = noteToNumber (N basicNote Nat) + 1

-- Allow naturals to prevail when possible
noteFromNumber preferred number = fromMaybe (fromJust (findAlt preferred)) (findAlt Nat)
  where
    possibleNotes = filter ((== (number `mod` 12)).noteToNumber) allNotes
    findAlt needed = find ((== needed).alt) possibleNotes

sameSound note1 note2 = noteToNumber note1 == noteToNumber note2

semitone upOrDown preferredAlteration = noteFromNumber preferredAlteration.upOrDown.noteToNumber

tone upOrDown preferredAlteration = semi.semi
  where semi = semitone upOrDown preferredAlteration

up = (+1)
down = flip (-) 1

-- Defaulting to Sharp, as it's not of interest
interval n toneOrSemitone note = (foldl (flip ($)) note.take n.repeat.toneOrSemitone up) Sharp

---- WithNotes typeclass

class WithNotes a where
  notes :: a -> [Note]

  tonic :: a -> Note
  tonic = head.notes

  isMajor :: a -> Bool
  isMajor = includesInterval (interval 2 tone)

  isMinor :: a -> Bool
  isMinor = includesInterval (interval 3 semitone)

  includesInterval :: (Note -> Note) -> a -> Bool
  includesInterval interval a = (any (sameSound ((interval.tonic) a)).notes) a

---- Pritty print and read

instance Show NoteAlteration where
  show Flat = "b"
  show Nat = ""
  show Sharp = "#"

instance Show Note where
  show (N basicNote alt) = show basicNote ++ show alt

instance Read Note where
  readsPrec _ string = map (\note -> (note , "")) (filter ((== string).show) allNotes)

readNote = (\s -> read s :: Note)
readNotes = map readNote

instance ToJSON Note where
  toJSON (N basicNote alteracion) = object [ "basicNote" .= show basicNote, "alt" .= show alteracion ]

instance Parsable Note where
  parseParam text = case (find ((== (unpack text)).show) allNotes) of
    Just note -> Right note
    Nothing -> Left "Nope"

instance FromJSON BasicNote where
  parseJSON (String o) = read <$> T.unpack <$> pure o

instance FromJSON NoteAlteration where
  parseJSON (String o) = read <$> T.unpack <$> pure o

instance FromJSON Note where
  parseJSON (Object o) = N <$>
                          o .: "basicNote" <*>
                          o .: "alt"