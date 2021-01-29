{-|
    Module      : Types
    Description : Derde checkpoint voor V2DeP: audio-synthesis en ringtone-parsing
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we audio genereren op basis van een abstracte representatie van noten.
    Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

    Deze module bevat enkele algemene polymorfe functies.
-}

module Util (zipWithL, zipWithR, comb, fst3, uncurry3) where

import Control.Applicative (liftA2)

-- | Version of ZipWith guaranteed to produce a list of the same length as the second input.
zipWithR :: (a -> b -> b) -> [a] -> [b] -> [b]
zipWithR _ _      []     = []
zipWithR _ []     bs     = bs
zipWithR f (a:as) (b:bs) = f a b : zipWithR f as bs

-- TODO Maak een `zipWithL` die als mirror-versie van `zipWithR` fungeert door de lengte gelijk te houden aan die van de eerste input in plaats van de tweede.

-- | Function zipWithL is a version of ZipWith guaranteed to produce a list of the same length as the first input.
-- takes 3 arguments, a function and 2 lists
zipWithL :: (a -> b -> a) -> [a] -> [b] -> [a]
zipWithL _ [] _ = [] -- als de 1e input leeg is geeft hij niks terug (base case 1)
zipWithL _ a [] = a -- als de 2e input leeg is geeft hij de rest van de 1e input terug (base case 2)
zipWithL func a b = func (head a) (head b) : zipWithL func (tail a) (tail b)

-- | Use a given binary operator to combine the results of applying two separate functions to the same value. Alias of liftA2 specialised for the function applicative.
comb :: (b -> b -> b) -> (a -> b) -> (a -> b) -> a -> b
comb = liftA2

-- TODO Maak een functie `fst3` die het eerste element van een 3-tuple teruggeeft.

-- | Function fst3 shows the first element of a 3-tuple
-- takes 1 argument, a 3-tuple
fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

-- TODO Maak een functie `uncurry3` die een functie met drie argumenten transformeert naar een functie die een 3-tuple als argument neemt.

-- | Function uncurry3 uses the elements of a 3-tuple for the arguments in a function
-- Takes 2 arguments, a function and a 3-tuple
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 func (a, b, c) = func a b c
