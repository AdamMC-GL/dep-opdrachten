{-|
    Module      : RTTL
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat een main-functie voor het lezen van user-supplied RTTL ringtones en het genereren van de behorende audio.
-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Types (Instrument)
import Instruments (defaultInstrument , defaultSquare , defaultTriangle , pop , twisted , bass , kick , noise)
import Data (ppk)
import IO (playRTTL)

-- Schrijf een main-functie die de gebruiker om een RTTL encoded String vraagt, het `instrumentMenu` print en vervolgens een getal overeenkomstig met een instrument. De string wordt met het gekozen element met `playRTTL` afgespeeld. Als er geen geldig instrument wordt meegegeven wordt `defaultInstrument` gepakt.
-- | The main menu gives a menu to the user to choose which instrument they want, and let them give a RTTL encoded string, then it uses the playRTTL function to start the whole process
main :: IO ()
--main = playRTTL defaultInstrument ppk
main = do
    putStrLn "Choose an instrument:"
    putStrLn instrumentMenu
    menu_choice <- getLine
    let maybe_instrument = chooseInstrument $ read menu_choice
    putStrLn "Give your RTTL encoded string:"
    rttl_string <- getLine
    case maybe_instrument of
        Just a -> playRTTL a rttl_string
        Nothing -> playRTTL defaultInstrument rttl_string

instrumentMenu :: String
instrumentMenu = unlines [ "1: sine"
                         , "2: square"
                         , "3: triangle"
                         , "4: pop"
                         , "5: twisted"
                         , "6: bass"
                         , "7: kick"
                         , "8: noise"
                         ]

-- Schrijf een functie `chooseInstrument` die een `Int` interpreteert tot een `Maybe Instrument` volgens de tabel hierboven.
-- | Just uses one big guard to connect a Int to the correct Instrument, when it isn't known, it returns Nothing
chooseInstrument :: Int -> Maybe Instrument
chooseInstrument x
    | x == 1 = Just defaultInstrument
    | x == 2 = Just defaultSquare
    | x == 3 = Just defaultTriangle
    | x == 4 = Just pop
    | x == 5 = Just twisted
    | x == 6 = Just bass
    | x == 7 = Just kick
    | x == 8 = Just noise
    | otherwise = Nothing
