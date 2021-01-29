module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where


-- | Een functie die de som van een lijst berekent
ex1 :: [Int] -> Int
ex1 [a] = a --base case, als de lijst 1 getal bevat wordt dat getal als int teruggegeven
ex1 a = (head a) + ex1 (tail a) --head van de lijst optellen met ex1 met als input de tail, de uikomst is de head van die tail

-- | Een functie die de elk element in de lijst verhoogt met 1. [1,2,3] -> [2,3,4]
ex2 :: [Int] -> [Int]
ex2 [a] = [a+1]
ex2 a = head a+1 : (ex2 (tail a))

-- | Een functie die elk element in de lijst vermenigvuldigt met -1, dus negatief naar positief maakt of visa versa. [1,-2,3] -> [-1,2,-3]
ex3 :: [Int] -> [Int]
ex3 [a] = [a*(-1)]
ex3 a = head a*(-1) : (ex3 (tail a))

-- | Een functie die 2 lijsten met elkaar concateneert. [1,2,3] [4,5,6] -> [1,2,3,4,5,6]
ex4 :: [Int] -> [Int] -> [Int]
ex4 [] b = b
ex4 a b = head a : ex4 (tail a) (b)

-- | Een functie die de elementen plekgewijs met elkaar optelt met een lijst als uitkomst. [1,2,3] [4,5,6] -> [5,7,9]
ex5 :: [Int] -> [Int] -> [Int]
ex5 [] [] = []
ex5 a b = (head a) + (head b) : (ex5 (tail a) (tail b))

-- | Een functie die de elementen plekgewijs met elkaar vermenigvuldigt met een lijst als uitkomst. [1,2,3] [4,5,6] -> [4,10,18]
ex6 :: [Int] -> [Int] -> [Int]
ex6 [] [] = []
ex6 a b = (head a) * (head b) : (ex6 (tail a) (tail b))

-- | Een functie dat het inwendig product van 2 lijsten berekent. [1,2,3] [4,5,6] = 1*4 + 2*5 + 3*6 = 32.
ex7 :: [Int] -> [Int] -> Int
ex7 [a] [b] = a
ex7 a b = ex1 (ex6 a b)