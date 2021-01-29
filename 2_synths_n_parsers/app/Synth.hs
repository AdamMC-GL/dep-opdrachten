{-|
    Module      : Synth
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat een main-functie voor het afspelen van multi-track audio vanuit abstracte noten-representatie.
-}

module Main (main) where

import Data (sandstorm', zombie')
import IO (playVLC)

-- TODO In deze file hoef je niets te doen, maar kan je eventuele eigen songs en instrumenten toevoegen.

main :: IO ()
main = playVLC [ ("sandstorm.wav"   , 120, sandstorm')
               , ("zombienation.wav",  70, zombie')
               ]
