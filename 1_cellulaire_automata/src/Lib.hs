{-|
    Module      : Lib
    Description : Tweede checkpoint voor V2DeP: cellulaire automata
    Copyright   : (c) Brian van der Bijl, 2020
    License     : BSD3
    Maintainer  : brian.vanderbijl@hu.nl

    In dit practicum gaan we aan de slag met 1D cellulaire automata [<https://mathworld.wolfram.com/Rule30.html>].
-}

module Lib where

import Data.Maybe (catMaybes) -- Niet gebruikt, maar deze kan van pas komen...
import Data.List (unfoldr)
import Data.Tuple (swap)

-- Om de state van een cellulair automaton bij te houden bouwen we eerst een set functies rond een `FocusList` type. Dit type representeert een 1-dimensionale lijst, met een
-- enkel element dat "in focus" is. Het is hierdoor mogelijk snel en makkelijk een enkele cel en de cellen eromheen te bereiken.

-- * FocusList

{- | The focussed list [0 1 2 ⟨3⟩ 4 5] is represented as @FocusList [3,4,5] [2,1,0]@. The first element (head) of the first list is focussed, and is easily and cheaply accessible.
 -   The items before the focus are placed in the backwards list in reverse order, so that we can easily move the focus by removing the focus-element from one list and prepending
 -   it to the other.
-}
data FocusList a = FocusList { forward :: [a]
                             , backward :: [a]
                             }
  deriving Show

-- De instance-declaraties mag je voor nu negeren.
instance Functor FocusList where
  fmap = mapFocusList

-- Enkele voorbeelden om je functies mee te testen:
intVoorbeeld :: FocusList Int
intVoorbeeld = FocusList [3,4,5] [2,1,0]

stringVoorbeeld :: FocusList String
stringVoorbeeld = FocusList ["3","4","5"] ["2","1","0"]

-- TODO Schrijf en documenteer een functie die een focus-list omzet in een gewone lijst. Het resultaat bevat geen focus-informatie meer, maar moet wel op de juiste volgorde staan.
-- toList intVoorbeeld ~> [0,1,2,3,4,5]

-- | Function toList that turns a focus list to a regular list
-- Takes 1 argument, a focus list
join [] b = b
join a b = head a : join (tail a) (b)
toList :: FocusList a -> [a]
toList (FocusList a b) = join (reverse b)(a)

-- TODO Schrijf en documenteer een functie die een gewone lijst omzet in een focus-list. Omdat een gewone lijst geen focus heeft moeten we deze kiezen; dit is altijd het eerste element.

-- | Function fromList that turns a regular list to a focus list.
-- Takes 1 argument, a list
fromList :: [a] -> FocusList a
fromList a = FocusList a []

-- | Move the focus one to the left
goLeft :: FocusList a -> FocusList a
goLeft (FocusList fw (f:bw)) = FocusList (f:fw) bw

-- TODO Schrijf en documenteer zelf een functie goRight die de focuslist een plaats naar rechts opschuift.

-- | Function goRight that shifts a focus list a place to the right
-- Takes 1 argument, a focus list
goRight :: FocusList a -> FocusList a
goRight (FocusList a b) = FocusList (tail a) (head a: b) 

-- TODO Schrijf en documenteer een functie leftMost die de focus geheel naar links opschuift.

-- | Function leftMost that shifts a focus list all the way to the left end
-- Takes 1 argument, a focus list
leftMost :: FocusList a -> FocusList a
leftMost (FocusList a []) = (FocusList a [])
leftMost (FocusList a b) = leftMost(goLeft (FocusList a b))

-- TODO Schrijf en documenteer een functie rightMost die de focus geheel naar rechts opschuift.

-- | Function rightMost that shifts a focus list all the way to the right end
-- Takes 1 argument, a focus list
rightMost :: FocusList a -> FocusList a
rightMost (FocusList [a] b) = (FocusList [a] b)
rightMost (FocusList a b) = rightMost(goRight (FocusList a b))

-- De functies goLeft en goRight gaan er impliciet vanuit dat er links respectievelijk rechts een cell gedefinieerd is. De aanroep `goLeft $ fromList [1,2,3]` zal echter crashen
-- omdat er in een lege lijst gezocht wordt: er is niets links. Dit is voor onze toepassing niet handig, omdat we bijvoorbeeld ook de context links van het eerste vakje nodig
-- hebben om de nieuwe waarde van dat vakje te bepalen (en dito voor het laatste vakje rechts).

-- TODO Schrijf en documenteer de functies totalLeft en totalRight die de focus naar links respectievelijk rechts opschuift; als er links/rechts geen vakje meer is, dan wordt een
-- lege (dode) cel teruggeven. Hiervoor gebruik je de waarde `mempty`, waar we met een later college nog op in zullen gaan. Kort gezegd zorgt dit ervoor dat de FocusList ook
-- op andere types blijft werken - je kan dit testen door totalLeft/totalRight herhaaldelijk op de `voorbeeldString` aan te roepen, waar een leeg vakje een lege string zal zijn.

-- [⟨░⟩, ▓, ▓, ▓, ▓, ░]  ⤚goLeft→ [⟨░⟩, ░, ▓, ▓, ▓, ▓, ░]

-- | Function totalLeft that shift a focus list place to the left, but can go beyond the bounds of the focus list by creating an empty when shifting past the bounds
-- Takes 1 argument, a focus list
totalLeft :: (Eq a, Monoid a) => FocusList a -> FocusList a
totalLeft (FocusList a []) = FocusList (mempty : a) []
totalLeft a = goLeft a 

-- | Function totalRight that shift a focus list place to the right, but can go beyond the bounds of the focus list by creating an empty when shifting past the bounds
-- Takes 1 argument, a focus list
totalRight :: (Eq a, Monoid a) => FocusList a -> FocusList a
totalRight (FocusList [a] b) = FocusList ([mempty]) (a:b)
totalRight a = goRight a 

-- TODO In de colleges hebben we kennis gemaakt met een aantal hogere-orde functies zoals `map`, `zipWith` en `fold[r/l]`. Hier zullen we equivalenten voor de FocusList opstellen.
-- De functies mapFocusList werkt zoals je zou verwachten: de functie wordt op ieder element toegepast, voor, op en na de focus. Je mag hier gewoon map voor gebruiken

-- | Function mapFocusList applies a function to every element in a focus list
-- Takes 2 arguments, the function and focus list
mapFocusList :: (a -> b) -> FocusList a -> FocusList b
mapFocusList func (FocusList a b) = FocusList (map (func) a)  (map (func) b)

-- TODO De functie zipFocusList zorgt ervoor dat ieder paar elementen uit de FocusLists als volgt met elkaar gecombineerd wordt:
-- [1, 2, ⟨3⟩,  4, 5]
-- [  -1, ⟨1⟩, -1, 1, -1]
--------------------------- (*)
-- [  -2, ⟨3⟩, -4, 5    ]

-- Oftewel: de megegeven functie wordt aangeroepen op de twee focus-elementen, met als resultaat het nieuwe focus-element. Daarnaast wordt de functie paarsgewijs naar
-- links/rechts doorgevoerd, waarbij gestopt wordt zodra een van beide uiteinden leeg is. Dit laatste is net als bij de gewone zipWith, die je hier ook voor mag gebruiken.

-- | Function zipFocusListWith applies a function to combine 2 focus lists and results a new focus list. ex: zipFocusListWith (+) [1, (2), 3] [(2), 3] -> [(4), 6]
-- Takes 3 arguments, the function and 2 focus lists
zipFocusListWith :: (a -> b -> c) -> FocusList a -> FocusList b -> FocusList c
zipFocusListWith func (FocusList a1 b1) (FocusList a2 b2) = FocusList (zipWith (func) a1 a2 ) (zipWith (func) b1 b2 )

-- TODO Het folden van een FocusList vergt de meeste toelichting: waar we met een normale lijst met een left fold en een right fold te maken hebben, moeten we hier vanuit de focus werken.
-- Vanuit de focus worden de elementen van rechts steeds gecombineerd tot een nieuw element, vanuit het element voor de focus gebeurt hetzelfde vanuit links. De twee resultaten van
-- beide sublijsten (begin tot aan focus, focus tot en met eind) worden vervolgens nog een keer met de meegegeven functie gecombineerd. Hieronder een paar voorbeelden:

-- foldFocusList (*) [0, 1, 2, ⟨3⟩, 4, 5] = (0 * (1 * 2)) * ((3 * 4) * 5)

-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = (0 - (1 - 2)) - ((3 - 4) - 5)
-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = (0 - (-1)) - ((-1) - 5)
-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = 1 - (-6)
-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = 7

-- Je kunt `testFold` gebruiken om je functie te testen. Denk eraan dat de backwards lijst achterstevoren staat, en waarschijnlijk omgekeerd moet worden.

-- | Function folFocusList folds a focus list by combining the elements through a function. First from the focus to the right and from the focus to the left, then combining both results with the function. 
-- Takes 2 arguments, the function and the focus list.
foldFocusList :: (a -> a -> a) -> FocusList a -> a
foldFocusList f (FocusList a b) = (f) (foldr1 (f) (reverse b)) (foldl (f) (head a) (tail a))

-- | Test function for the behaviour of foldFocusList.
testFold :: Bool
testFold = and [ foldFocusList (+) intVoorbeeld     == 15
               , foldFocusList (-) intVoorbeeld     == 7
               , foldFocusList (++) stringVoorbeeld == "012345"
               ]

-- * Cells and Automata

-- Nu we een redelijk complete FocusList hebben kunnen we gaan kijken naar daadwerkelijke celulaire automata, te beginnen met de Cell.

-- | A cell can be either on or off, dead or alive. What basic type could we have used instead? Why would we choose to roll our own equivalent datatype?
data Cell = Alive | Dead deriving (Show, Eq)

-- De instance-declaraties mag je voor nu negeren.
instance Semigroup Cell where
  Dead <> x = x
  Alive <> x = Alive

instance Monoid Cell where
  mempty = Dead

-- | The state of our cellular automaton is represented as a FocusList of Cells.
type Automaton = FocusList Cell

-- | Start state, per default, is a single live cell.
start :: Automaton
start = FocusList [Alive] []

-- | Alternative start state with 5 alive cells, for shrinking rules.
fiveAlive :: Automaton
fiveAlive = fromList $ replicate 5 Alive

-- | A rule [<https://mathworld.wolfram.com/Rule30.html>] is a mapping from each possible combination of three adjacent cells to the associated "next state".
type Context = [Cell]
type Rule = Context -> Cell

-- * Rule Iteration

-- TODO Schrijf en documenteer een functie safeHead die het eerste item van een lijst geeft; als de lijst leeg is wordt een meegegeven default values teruggegeven.

-- | Function safeHead that returns the first item of a list, or a default value if the list is empty. 
-- Takes 2 arguments, the default value and the list.
safeHead :: a        -- ^ Default value
         -> [a]      -- ^ Source list
         -> a
safeHead defVal [] = defVal
safeHead defVal b = head b

-- TODO Schrijf en documenteer een functie takeAtLeast die werkt als `take`, maar met een extra argument. Als de lijst lang genoeg is, bijvoorbeeld
-- `takeAtLeast 3 "0" ["1","2","3","4","5"]` dan werkt de functie hetzelfde als `take` en worden de eerste `n` (hier 3) elementen teruggegeven.
-- Als dat niet zo is dan worden zoveel mogelijk elementen teruggegeven, en wordt de lijst daarna tot de gevraagde lengte aangevuld met een
-- meegegeven default-waarde: `takeAtLeast 3 "0" ["1"] ~> ["1", "0", "0"]`.

-- | Function takeAtLeast takes the first n values of a list, if n is larger then the list length a default value is put in place of the remaining
-- Takes 3 arguments, n (length you want to take) the default value and the list
takeAtLeast :: Int   -- ^ Number of items to take
            -> a     -- ^ Default value added to the right as padding
            -> [a]   -- ^ Source list
            -> [a]
takeAtLeast lenVal defVal a = take lenVal (a++repeat defVal)

-- TODO Schrijf en documenteer een functie context die met behulp van takeAtLeast de context van de focus-cel in een Automaton teruggeeft. Niet-gedefinieerde cellen zijn per definitie Dead.
-- | Function context shows the context of a focus list, showing the focus and its neighbours
-- Takes 1 argument, a focus list
context :: Automaton -> Context
context (FocusList a b) = (takeAtLeast 1 Dead b) ++ (takeAtLeast 2 Dead a) 

-- TODO Schrijf en documenteer een functie expand die een Automaton uitbreid met een dode cel aan beide uiteindes. We doen voor deze simulatie de aanname dat de "known universe"
-- iedere ronde met 1 uitbreid naar zowel links als rechts.

-- | Function expand expands a Automaton by a dead cell on each end
-- Takes 1 argument, an automaton (focus list)
expand :: Automaton -> Automaton
expand (FocusList a b) = FocusList (a++[Dead]) (b++[Dead]) 

-- | A sequence of Automaton-states over time is called a TimeSeries.
type TimeSeries = [Automaton]

-- TODO Voorzie onderstaande functie van interne documentatie, d.w.z. zoek uit en beschrijf hoe de recursie verloopt. Zou deze functie makkelijk te schrijven zijn met behulp van
-- de hogere-orde functies die we in de les hebben gezien? Waarom wel/niet?

{- |
Iterate a given rule n times, given a start state. The result will be a sequence of states from start to n.
returns a series of states. 3 input variables: r, n, s

First it looks at the base case, which is when n = 0, and it will return the initial state,
else it starts the recursion. first the initial state s is put in front of the outcome of the recursion.
The recursion happens with the same rule r as input, n-1 and a modified s.

s is modified by first expanding it with 2 dead cells each side, then it is shifted all the way to the left. This is then put into the applyRule function.

The applyRule is also a recursive function with the base case being the focuslist input having shifted all the way to the right,
else the rule is applied to the context of z, which is the focus and 2 surrounding cells, the outcome is then put at the end of the list
the focus then shifts one to the right and the process is repeated until the base case

the outcome of applyRule is a list of cells after having applied the rule to all of them, in other words: a state
This list is put into a focus list and given to the next recursion.
In the next iteration this state is again give to applyRule, changing it to the next state. This process is done until the base case

So the function iterateRule has a series of states as output, describing all the states of a series of cell over a given time. Each state is determined by the rule-}

iterateRule :: Rule          -- ^ The rule to apply
            -> Int           -- ^ How many times to apply the rule
            -> Automaton     -- ^ The initial state
            -> TimeSeries
iterateRule r 0 s = [s] -- return given state when n is 0 (base case)
iterateRule r n s = s : iterateRule r (pred n) (fromList $ applyRule $ leftMost $ expand s) -- recursion with same rule, n-1, and a modified s as input. s with 2 dead cell expanded, shifted to the left, put in applyRule and turned into a focusList
  where applyRule :: Automaton -> Context
        applyRule (FocusList [] bw) = [] -- when the forward list is empty it returns empty, so when the focuslist is shifted to all the way to the right (base case)
        applyRule z = r (context z) : applyRule (goRight z) -- rule is applied to (context z) followed by shifting z to the right and applying the rule, repeats until base case

-- | Convert a time-series of Automaton-states to a printable string.
showPyramid :: TimeSeries -> String
showPyramid zs = unlines $ zipWith showFocusList zs $ reverse [0..div (pred w) 2]
  where w = length $ toList $ last zs :: Int
        showFocusList :: Automaton -> Int -> String
        showFocusList z p = replicate p ' ' <> concatMap showCell (toList z)
        showCell :: Cell -> String
        showCell Dead  = "░"
        showCell Alive = "▓"

-- TODO Vul de functie rule30 aan met de andere 7 gevallen. Je mag de voorbeeldregel aanpassen/verwijderen om dit in minder regels code te doen. De underscore _ is je vriend.

-- | Function rule30 and all its possible answers
rule30 :: Rule
rule30 [Dead, Dead, Dead] = Dead -- 1
rule30 [Dead, _, _]  = Alive -- 2, 3, 4. if first one is dead but any of the next ones are alive, outcome is alive
rule30 [Alive, Alive, Alive] = Dead -- 5
rule30 [Alive, Dead, Alive] = Dead -- 6
rule30 [Alive, Alive, Dead] = Dead -- 7
rule30 [Alive, Dead, Dead] = Alive -- 8

-- ...

-- Je kan je rule-30 functie in GHCi testen met het volgende commando:
-- putStrLn . showPyramid . iterateRule rule30 15 $ start

-- De verwachte uitvoer is dan:
{-             ▓
              ▓▓▓
             ▓▓░░▓
            ▓▓░▓▓▓▓
           ▓▓░░▓░░░▓
          ▓▓░▓▓▓▓░▓▓▓
         ▓▓░░▓░░░░▓░░▓
        ▓▓░▓▓▓▓░░▓▓▓▓▓▓
       ▓▓░░▓░░░▓▓▓░░░░░▓
      ▓▓░▓▓▓▓░▓▓░░▓░░░▓▓▓
     ▓▓░░▓░░░░▓░▓▓▓▓░▓▓░░▓
    ▓▓░▓▓▓▓░░▓▓░▓░░░░▓░▓▓▓▓
   ▓▓░░▓░░░▓▓▓░░▓▓░░▓▓░▓░░░▓
  ▓▓░▓▓▓▓░▓▓░░▓▓▓░▓▓▓░░▓▓░▓▓▓
 ▓▓░░▓░░░░▓░▓▓▓░░░▓░░▓▓▓░░▓░░▓
▓▓░▓▓▓▓░░▓▓░▓░░▓░▓▓▓▓▓░░▓▓▓▓▓▓▓ -}

-- * Rule Generation

-- Er bestaan 256 regels, die we niet allemaal met de hand gaan uitprogrammeren op bovenstaande manier. Zoals op de genoemde pagina te zien is heeft het nummer te maken met binaire
-- codering. De toestand van een cel hangt af van de toestand van 3 cellen in de vorige ronde: de cel zelf en diens beide buren (de context). Er zijn 8 mogelijke combinaties
-- van 3 van dit soort cellen. Afhankelijke van het nummer dat een regel heeft mapt iedere combinatie naar een levende of dode cel.

-- TODO Definieer allereerst een constante `inputs` die alle 8 mogelijke contexts weergeeft: [Alive,Alive,Alive], [Alive,Alive,Dead], etc.
-- Je mag dit met de hand uitschrijven, maar voor extra punten kun je ook een lijst-comprehensie of andere slimme functie verzinnen.

-- | Function inputs shows every possible combination of a context
inputs :: [Context]
inputs = [[a,b,c] |a <- [Alive, Dead],  b <- [Alive, Dead], c <- [Alive, Dead]]

-- | If the given predicate applies to the given value, return Just the given value; in all other cases, return Nothing.
guard :: (a -> Bool) -> a -> Maybe a
guard p x | p x = Just x
          | otherwise = Nothing


-- | Function binary converts an int to a binairy representation [Bool]. 8 -> 00001000 -> [0, 0, 0, 0, 1, 0, 0, 0]
binary :: Int -> [Bool]
binary = map toEnum . reverse . take 8 . (++ repeat 0)
       . unfoldr (guard (/= (0,0)) . swap . flip divMod 2)

-- TODO Schrijf en documenteer een functie mask die, gegeven een lijst Booleans en een lijst elementen alleen de elementen laat staan die (qua positie) overeenkomen met een True.
-- Je kan hiervoor zipWith en Maybe gebruiken (check `catMaybes` in Data.Maybe) of de recursie met de hand uitvoeren.

-- | Function mask masks 2 lists by comparing a list of bools and a list of values and keeps the values where True positionally sits in the bool list
-- takes 2 arguments, a list of bools and a list
mask :: [Bool] -> [a] -> [a]
mask [] [] = [] -- base case: if the input lists are empty
mask boolList lst = if (head boolList) -- if the head of the boolList is True
then head lst : mask (tail boolList) (tail lst) -- then the head of the int list is added, then next recursion step
else mask (tail boolList) (tail lst) -- else adding the head if the int list is skipped and the next recursion begins

-- TODO Combineer `mask` en `binary` met de library functie `elem` en de eerder geschreven `inputs` tot een rule functie. Denk eraan dat het type Rule een short-hand is voor een
-- functie-type, dus dat je met 2 argumenten te maken hebt. De Int staat hierbij voor het nummer van de regel, dat je eerst naar binair moet omrekenen; de Context `input` is
-- waarnaar je kijkt om te zien of het resultaat met de gevraagde regel Dead or Alive is. Definieer met `where` subset van `inputs` die tot een levende danwel dode cel leiden.
-- Vergeet niet je functie te documenteren.

-- | Function rule returns an alive or dead cell based on the rule. n is turned into a binary and is masked with all possible inputs. What remains are the possible inputs that mean 'alive'. 
-- then elem input looks at if the given input is within these 'alive' inputs, if so it returns True and thus will the function return an Alive cell.
rule :: Int -> Rule
rule n input = if elem input (mask (binary n) inputs) -- based on n (rule number) all possible inputs that give Alive as a result is determined, if the given input is in the inputs the if statement is True
then Alive -- if True the cell is Alive
else Dead -- if False the cell is Dead


{- Je kan je rule-functie in GHCi testen met variaties op het volgende commando:

   putStrLn . showPyramid . iterateRule (rule 18) 15 $ start

                  ▓
                 ▓░▓
                ▓░░░▓
               ▓░▓░▓░▓
              ▓░░░░░░░▓
             ▓░▓░░░░░▓░▓
            ▓░░░▓░░░▓░░░▓
           ▓░▓░▓░▓░▓░▓░▓░▓
          ▓░░░░░░░░░░░░░░░▓
         ▓░▓░░░░░░░░░░░░░▓░▓
        ▓░░░▓░░░░░░░░░░░▓░░░▓
       ▓░▓░▓░▓░░░░░░░░░▓░▓░▓░▓
      ▓░░░░░░░▓░░░░░░░▓░░░░░░░▓
     ▓░▓░░░░░▓░▓░░░░░▓░▓░░░░░▓░▓
    ▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓
   ▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓

   putStrLn . showPyramid . iterateRule (rule 128) 10 $ fiveAlive

               ▓▓▓▓▓
              ░░▓▓▓░░
             ░░░░▓░░░░
            ░░░░░░░░░░░
           ░░░░░░░░░░░░░
          ░░░░░░░░░░░░░░░
         ░░░░░░░░░░░░░░░░░
        ░░░░░░░░░░░░░░░░░░░
       ░░░░░░░░░░░░░░░░░░░░░
      ░░░░░░░░░░░░░░░░░░░░░░░
     ░░░░░░░░░░░░░░░░░░░░░░░░░

   Als het goed is zal `stack run` nu ook werken met de voorgeschreven main functie; experimenteer met verschillende parameters en zie of dit werkt.
-}
