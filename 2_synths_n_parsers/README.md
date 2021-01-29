# Synthesisers en Parsers

In dit practicum gaan we aan de slag met audio-synthesis. We gaan audio genereren op basis van een abstracte representatie van noten en instrumenten. Daarnaast gaan we tekst in het RTTL formaat parsen tot deze abstracte representatie.

## Gratis punten (20 punten)
- Voor het correct inleveren van een GitLab link krijg je 5 punten.
- Voor het gebruik van FeedPulse kun je 15 punten verdienen.

## `src/Util.hs` (15 punten)
- Maak een `zipWithL` die als mirror-versie van `zipWithR` fungeert door de lengte gelijk te houden aan die van de eerste input in plaats van de tweede. (5 punten)
- Maak een functie `fst3` die het eerste element van een 3-tuple teruggeeft. (5 punten)
- Maak een functie `uncurry3` die een functie met drie argumenten transformeert naar een functie die een 3-tuple als argument neemt. (5 punten)

## `src/Types.hs` (50 punten)
- Maak instances voor `Sound` voor `Semigroup` en `Monoid`. De monoid-operatie staat in dit geval voor het sequentieel (achter elkaar) combineren van audiofragmenten. Het is van belang dat `IntFrames` alleen met `IntFrames` worden gecombineerd, en dito voor `FloatFrames`. Bij twee verschillende gevallen moet je beiden naar hetzelfde formaat converteren, idealiter `FloatFrames`. Wat is een leeg audiofragment in deze context? (15 punten)
- Maak een operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert door de geluiden tegelijk af te spreken. Dit mag door de frames als in een `zipWith (+)` samen te voegen, maar in dit geval wordt niet de kortste maar de langste lijst aangehouden (in dat geval wordt enkel het aanwezige geluid weergegeven). Je hoeft deze operator alleen op `FloatFrames` te matchen, de laatste regel converteert alles hierheen als een of beide argumenten in `IntFrames` staan. (10 punten)
- Maak een functie `asIntSOund` die als inverse van `asFloatSound` fungeert. (5 punten)
- Maak een functie `modifyInstrument) die een `Modifier` met een `Instrument` combineert. Gebruik een lambda om een nieuw instrument terug te geven, waarbij de functie in de modifier met de functie in het instrument gecomposed wordt. (10 punten)
- Maak een functie `arrange` die de functie in het meegegeven `Instrument` toepast op de frequentie en duur. Het resultaat wordt als `Sound` verpakt. (10 punten)

## `src/Instruments.hs` (25 punten)
- Maak een functie silence die een gegeven aantal seconden aan stilte genereert. Hiervoor kun je een lijst met het juiste aantal (`sampleRate * seconden`) keer `0.0` teruggeven, verpakt tot het `Sound`-datatype. (10 punten)
- Maak een `Instrument` `defaultSquare` gebaseerd op de `squareWave`, gecombineerd met een `attack` en `release` `Modifier`. (5 punten)
- Maak een `Instrument` `defaultTrangle` gebaseerd op de `triangleWave`, gecombineerd met een `attack` en `release` `Modifier`. (5 punten)
- Maak een functie `pad` die een lijst noten "pad" met vooraf en achteraf quarter-note pauze. Deze wordt gebruikt in de `generateWave` functie om de WAV-export beter te laten klinken. (5 punten)

## `src/Parsers.hs` (60 punten)
- Schrijf een `Parser` `pComplementCharSet` die een lijst van karakters meekrijgt (ook wel een `String`) en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit. (10 punten)
- Schrijf een `Parser` `pString` die een gegegeven `String` probeert te parsen. Gebruik hiervoor `do` notatie; parse een enkele letter met `pCharSet` en parse de rest recursief met `pString`; combineer beide waarden weer voordat je deze `return`t. Vergeet niet een geval voor een lege `String`  te schrijven. (10 punten)
- Schrijf een `Parser` `pOpttional` die gegeven een `Parser` optioneel maakt. Als de `Parser` slaagt wordt een `Just` value teruggegeven, zo niet wordt `Nothing` ge`return`ed. Je kunt hiervoor gebruik maken van `mplus` uit `Control.Monad`. (10 punten)
- Combineer `pRepeat` en `pCharSet` tot een `Parser` die een getal als `String` leest, en roep hier `read` op aan om een `Int` terug te geven. (10 punten)
- Schrijf een `Parser` `pOctave`. Je kunt `toEnum` gebruiken om een `Int` naar een `Octave` te casten. (5 punten)
- Pas deze `Parser` aan zodat de de titel uit de RTTL string wordt gehaald en in de plaats van PLACEHOLDER wordt teruggegeven. (5 punten)
- Schrijf een functie `parse` die `pRTTL` aanroept. Bedenk hierbij dat een `Parser` eigenlijk niet meer is dan een `StateT` met een `Maybe` erin. (10 punten)

## `app/RTTL.hs` (20 punten)
- Schrijf een main-functie die de gebruiker om een RTTL encoded String vraagt, het `instrumentMenu` print en vervolgens een getal overeenkomstig met een instrument. De string wordt met het gekozen element met `playRTTL` afgespeeld. Als er geen geldig instrument wordt meegegeven wordt `defaultInstrument` gepakt. (10 punten)
- Schrijf een functie `chooseInstrument` die een `Int` interpreteert tot een `Maybe Instrument` volgens de tabel hierboven. (10 punten)

## Bonus!
- Voel je vrij je eigen instrumenten toe te voegen door te experimenteren met generators en modifiers.
- Voel je vrij je eigen songs toe te voegen, in RTTL-Strings of `Tracks` met noten.
  - De beste toevoegingen kunnen leiden tot extra punten en worden toegevoegd in het practicum van volgend jaar.
