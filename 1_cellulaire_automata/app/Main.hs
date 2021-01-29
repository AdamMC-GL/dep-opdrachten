module Main where

import Lib
import Text.Read
import Data.Maybe (fromMaybe)

main :: IO ()
main = do putStrLn "Geef het getal van de gewenste rule? [18]"
          nr <- fromMaybe 18 . readMaybe <$> getLine
          putStrLn "Geef het gewenste aantal iteraties? [15]"
          n <- fromMaybe 15 . readMaybe <$> getLine
          putStrLn $ showPyramid $ iterateRule (rule nr) n start
