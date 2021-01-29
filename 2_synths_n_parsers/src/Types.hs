{-|
    Module      : Types
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat alle type-declaraties, instanties en type-gerelateerde functies, zodat we deze makkelijk in meerdere modules kunnen importeren.
-}

{-# LANGUAGE TypeApplications #-}

module Types ( Beats, Hz, Samples, Seconds, Semitones, Track, Ringtone
             , Tone(..), Octave(..), Duration(..), Note(..)
             , Sound, floatSound, intSound, (<+>), asFloatSound, asIntSound, getAsFloats, getAsInts
             , Instrument, instrument, Modifier, modifier, modifyInstrument, arrange
             ) where

import Util
import Data.Int (Int32)


type Pulse = [Float]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Ringtone = String

data Tone = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Enum, Eq, Show)
data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Enum, Eq, Show)
data Duration = Full | Half | Quarter | Eighth | Sixteenth | Thirtysecond | Dotted Duration deriving (Eq, Show)
data Note = Pause Duration | Note Tone Octave Duration deriving (Eq, Show)

data Sound = IntFrames [Int32] | FloatFrames [Float]
  deriving Show

floatSound :: [Float] -> Sound
floatSound = FloatFrames

intSound :: [Int32] -> Sound
intSound = IntFrames

instance Eq Sound where
  (FloatFrames xs) == (FloatFrames ys) = all ((<  0.001) . abs) $ zipWith (-) xs ys
  (IntFrames xs) == (IntFrames ys)     = all ((< 10    ) . abs) $ zipWith (-) xs ys
  _ == _                               = False

-- TODO Maak instances voor `Sound` voor `Semigroup` en `Monoid`. De monoid-operatie staat in dit geval voor het sequentieel (achter elkaar) 
-- combineren van audiofragmenten. Het is van belang dat `IntFrames` alleen met `IntFrames` worden gecombineerd, en dito voor `FloatFrames`. 
-- Bij twee verschillende gevallen moet je beiden naar hetzelfde formaat converteren, idealiter `FloatFrames`. Wat is een leeg audiofragment in deze context?

-- | instance that combines 2 Intframes audiofragments, if one of them isnt an int both turn into a float is combined to a FloatFrames 
instance Semigroup Sound where
  (IntFrames x) <> (IntFrames y) = IntFrames (x ++ y)   -- het combineren van 2 lijsten van 2 dezelfde datatypes
  x <> y = FloatFrames (getAsFloats x ++ getAsFloats y) -- Als het niet beide een intFrame is dan zet je ze alle bij naar FloatFrames

instance Monoid Sound where
    mempty = IntFrames []


-- TODO Maak een operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert door de geluiden tegelijk af te spreken. 
-- Dit mag door de frames als in een `zipWith (+)` samen te voegen, maar in dit geval wordt niet de kortste maar de langste lijst aangehouden (in dat geval wordt enkel het aanwezige geluid weergegeven).
-- Je hoeft deze operator alleen op `FloatFrames` te matchen, de laatste regel converteert alles hierheen als een of beide argumenten in `IntFrames` staan.

-- Combining 2 sounds by adding each float together by pair, sound stays as long as longest input sound by using that list.

-- | Function (<+>) Combines 2 sounds, output is as long as the longest list
(<+>) :: Sound -> Sound -> Sound 
(FloatFrames a) <+> (FloatFrames b) = if length a > length b
then FloatFrames (zipWithL (+) a b) 
else FloatFrames (zipWithR (+) a b) 

a <+> b = asFloatSound a <+> asFloatSound b

asFloatSound :: Sound -> Sound
asFloatSound (IntFrames fs) = floatSound $ map ( (/ fromIntegral (div (maxBound @Int32 ) 2 )) . fromIntegral ) fs
asFloatSound fframe = fframe

-- TODO Maak een functie `asIntSOund` die als inverse van `asFloatSound` fungeert.
asIntSound :: Sound -> Sound
asIntSound = undefined

getAsFloats :: Sound -> [Float]
getAsFloats sound = case asFloatSound sound of
  (FloatFrames ffs) -> ffs
  _ -> error "asFloatSound did not return FloatFrames"

getAsInts :: Sound -> [Int32]
getAsInts sound = case asIntSound sound of
  (IntFrames ifs) -> ifs
  _ -> error "asIntSound did not return IntFrames"

type Track = (Instrument, [Note])

newtype Instrument = Instrument (Hz -> Seconds -> Pulse)

instrument :: (Hz -> Seconds -> Pulse) -> Instrument
instrument = Instrument

newtype Modifier = Modifier (Pulse -> Pulse)

modifier :: (Pulse -> Pulse) -> Modifier
modifier = Modifier

instance Semigroup Modifier where
  (Modifier m1) <> (Modifier m2) = Modifier $ m1 . m2

-- TODO Maak een functie `modifyInstrument) die een `Modifier` met een `Instrument` combineert. Gebruik een lambda om een nieuw instrument terug te geven, waarbij de functie in de modifier met de functie in het instrument gecomposed wordt.

-- | Function modifyInstrument combines a modifier and an instrument, putting a modifier on said instrument
modifyInstrument :: Instrument -> Modifier -> Instrument
modifyInstrument (Instrument func1) (Modifier func2) = Instrument $ \hertz seconds -> func2 $ func1 hertz seconds

-- TODO Maak een functie `arrange` die de functie in het meegegeven `Instrument` toepast op de frequentie en duur. Het resultaat wordt als `Sound` verpakt.

-- | Function arrange applies a frequency and duration on an instrument to create a sound
arrange :: Instrument -> Hz -> Seconds -> Sound
arrange (Instrument func) freq sec = floatSound $ func freq sec
