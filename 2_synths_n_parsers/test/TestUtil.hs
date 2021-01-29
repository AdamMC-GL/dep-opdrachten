module Main (main) where

import Test.Hspec
import Util

main :: IO ()
main = mapM_ hspec [test_fst3, test_uncurry3, test_zipWithL]

abc_formule :: Float -> Float -> Float -> (Float, Float)
abc_formule a b c = ((-b + sqrt(b**2 - 4*a*c)) / 2*a, (-b + sqrt(b**2 - 4*a*c)) / 2*a)

test_fst3 :: Spec
test_fst3 = do
  describe "Util.fst3" $ do
    it "takes the first element of a three-tuple" $ do
      fst3 (6 :: Int, -7 :: Int, -3 :: Int) `shouldBe` 6

test_uncurry3 :: Spec
test_uncurry3 = do
  describe "Util.uncurry3" $ do
    it "converts a function requiring three arguments to one requiring a three-tuple" $ do
      uncurry3 abc_formule (6, -7, -3) `shouldBe` abc_formule 6 (-7) (-3)

test_zipWithL :: Spec
test_zipWithL = do
  describe "Util.zipWithL" $ do
    it "functions as normal zipWith for same-length lists" $ do
      zipWithL @Int (+) [1..5] [9..13] `shouldBe` zipWith (+) [1..5] [9..13]

    it "takes the length of the first list" $ do
      zipWithL @Int const [1..5] [] `shouldBe` [1..5]
