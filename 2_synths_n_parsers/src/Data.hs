{-|
    Module      : Data
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat voorbeelden van RTTL-gecodeerde strings en de abstracte noten-representatie.
-}

module Data (sandstorm', zombie', sandstorm, children, axelf, ppk, numberone, rick) where

import Types (Track, Ringtone, Tone(..), Note(..), Octave(..), Duration(..))
import Instruments (defaultInstrument, noise, kick, twisted)

import Data.Semigroup (stimes)

-- TODO Voel je vrij je eigen songs toe te voegen, in RTTL-Strings of Tracks met noten.
-- De beste toevoegingen kunnen leiden tot extra punten en worden toegevoegd in het practicum van volgend jaar.

sandstorm' :: [Track]
sandstorm' = [(defaultInstrument, [Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Eighth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Eighth, Note E Four Sixteenth, Note E Four Sixteenth, Note E Four Sixteenth, Note E Four Sixteenth, Note E Four Sixteenth, Note E Four Sixteenth, Note E Four Eighth, Note D Four Sixteenth, Note D Four Sixteenth, Note D Four Sixteenth, Note D Four Sixteenth, Note D Four Sixteenth, Note D Four Sixteenth, Note D Four Eighth, Note A Three Eighth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Quarter, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Eighth, Note E Four Eighth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Quarter, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Sixteenth, Note B Three Quarter])]

zombie' :: [Track]
zombie' = [(noise, zSnare), (kick, zKick), (kick, zKick), (kick, zKick), (twisted, zNotes)]
  where zKick = stimes (7 :: Int) [Note B One Eighth, Pause Eighth] <> [Pause Half] <> stimes (5 :: Int) [Note B One Eighth, Pause Eighth]
        zSnare = stimes (7 :: Int) [Pause Eighth, Note B Three Sixteenth, Pause Sixteenth] <> [Pause Half] <> stimes (5 :: Int) [Pause Eighth, Note B Three Sixteenth, Pause Sixteenth]
        zNotes = [Note B Two Sixteenth, Note D Three Sixteenth, Note E Three Sixteenth, Note FSharp Three Sixteenth, Note B Two Sixteenth, Pause Sixteenth, Note B Two Eighth, Note B Two Sixteenth, Note D Three Sixteenth, Note E Three Sixteenth, Note FSharp Three Sixteenth, Note G Three Sixteenth, Note FSharp Three Sixteenth, Note D Three Sixteenth, Note E Three Eighth, Pause Eighth, Note D Three Sixteenth, Pause Sixteenth, Note FSharp Three Sixteenth, Note B Two Eighth, Pause Half, Note B Two Sixteenth, Note D Three Sixteenth, Note E Three Sixteenth, Note FSharp Three Sixteenth, Note B Two Sixteenth, Pause Sixteenth, Note B Two Eighth, Note B Two Sixteenth, Note D Three Sixteenth, Note E Three Sixteenth, Note FSharp Three Sixteenth, Note G Three Sixteenth, Note FSharp Three Sixteenth, Note D Three Sixteenth, Note E Three Eighth, Pause Eighth, Note D Three Sixteenth, Pause Sixteenth, Note FSharp Three Sixteenth, Note B Two Eighth]

sandstorm, ppk, axelf, children, numberone, rick :: Ringtone
sandstorm = "Sandstorm:d=16, o=3, b=120:16b3 16b3 16b3 16b3 8b3 16b3 16b3 16b3 16b3 16b3 16b3 8b3 16e4 16e4 16e4 16e4 16e4 16e4 8e4 16d4 16d4 16d4 16d4 16d4 16d4 8d4 8a3 16b3 16b3 16b3 16b3 4b3 16b3 16b3 16b3 16b3 8b3 8e4 16b3 16b3 16b3 16b3 4b3 16b3 16b3 16b3 16b3 4b3"
ppk = "PPKResur:d=4, o=6, b=140:2d, 8e, 8f, 8e, 2d, 8e, 8f, 8e, 8d, 8e, 8f, 8e, 8d, 8e, 8f, 8e, 8d, 8e, 8f, 8e, d, a5, 1c, 1p, a#5, f, 8g, 8f, 8e, 2f, 8g, 8f, 8e, 8f, 8g, 8f, 8e, 8f, 8g, 8f, 8e, 8f, 8g, 8f, 8e, f, e, 16f, 16e, 2d"
axelf = "axelf:d=4, o=5, b=117:f#, 8a., 8f#, 16f#, 8a#, 8f#, 8e, f#, 8c.6, 8f#, 16f#, 8d6, 8c#6, 8a, 8f#, 8c#6, 8f#6, 16f#, 8e, 16e, 8c#, 8g#, f#."
children = "Children:d=4, o=4, b=137:8p, f.5, 1p, g#5, 8g5, d#.5, 1p, g#5, 8g5, c.5, 1p, g#5, 8g5, g#., 1p, 16f, 16g, 16g#, 16c5, f.5, 1p, g#5, 8g5, d#.5, 1p, 16c#5, 16c5, c#5, 8c5, g#, 2p, g., g#, 8c5, f."
rick = "rickroll:d=4, o=5, b=120:16c, 16d, 16f, 16d, 16a., 16p, 32p, 8a, 16p, g., 16c, 16d, 16f, 16d, 16g., 16p, 32p, 8g, 16p, 8f., 16e, 8d, 16c, 16d, 16f, 16d, f, 8g, 8e., 16d, 8c, 8c4, 8c, 8g, 8p, 2f, 16c, 16d, 16f, 16d, 16a., 16p, 32p, 8a, 16p, g., 16c, 16d, 16f, 16d, c6, 8e, 8f., 16e, 8d, 16c, 16d, 16f, 16d, f, 8g, 8e., 16d, 8c, 8c4, 8c, 8g, 8p, 2f"
numberone = "NumberOne:d=16, o=5, b=168:4f., 8c6, 16b5, 16c6, 16b5, 16c6, 8b5, 8c6, 4g#5, 4f., 8f, 8g#5, 8c6, 4c#6, 4g#5, 4c#6, 4d#6, 8c6, 8c#6, 8c6, 8c#6, 2c6"
