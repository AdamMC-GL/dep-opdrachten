module Main (main) where

import Test.Hspec
import Types

main :: IO ()
main = do putStrLn ""
          putStrLn "\ESC[31mAls hier meerdere tests falen, begin met test_asIntSound\ESC[0m"
          mapM_ hspec [test_asIntSound, test_semigroup, test_monoid, test_parallel, test_modifyInstrument]

test_semigroup :: Spec
test_semigroup = do
  describe "instance Semigroup Sound" $ do
    it "concatenates IntFrames" $ do
      intSound [1, 2, 3] <> intSound [4, 5, 6] `shouldBe` intSound [1, 2, 3, 4, 5, 6]
    it "concatenates FloatFrames" $ do
      floatSound [0.1, 0.2, 0.3] <> floatSound [0.4, 0.5, 0.6] `shouldBe` floatSound [0.1, 0.2, 0.3, 0.4, 0.5, 0.6]
    it "obeys the associativity law" $ do
      intSound [1, 2, 3] <> (intSound [4, 5, 6] <> intSound [7, 8, 9]) `shouldBe`
        (intSound [1, 2, 3] <> intSound [4, 5, 6]) <> intSound [7, 8, 9]

test_monoid :: Spec
test_monoid = do
  describe "instance Monoid Sound" $ do
    it "provides a mempty that serves as the left-identity" $ do
      asFloatSound mempty <> floatSound [0.1, 0.2, 0.3] `shouldBe` floatSound [0.1, 0.2, 0.3]
      asIntSound mempty <> intSound [1, 2, 3] `shouldBe` intSound [1, 2, 3]
    it "provides a mempty that serves as the right-identity" $ do
      floatSound [0.1, 0.2, 0.3] <> asFloatSound mempty `shouldBe` floatSound [0.1, 0.2, 0.3]
      intSound [1, 2, 3] <> asIntSound mempty `shouldBe` intSound [1, 2, 3]

test_parallel :: Spec
test_parallel = do
  describe "Types.(<+>)" $ do
    it "combines two sounds correctly" $ do
      asFloatSound (floatSound [0.1, 0.2, 0.3] <+> floatSound [0.9, 0.7, 0.5]) `shouldBe` (floatSound [1.0, 0.9, 0.8])
      asIntSound (intSound [1, 2, 3] <+> intSound [9, 7, 5]) `shouldBe` (intSound [10, 9, 8])
    it "does not clip sounds (takes the longest fragment when two sounds of different length are combined)" $ do
      asFloatSound (floatSound [0.1, 0.2, 0.3, 0.4] <+> floatSound [0.9, 0.7, 0.5]) `shouldBe` (floatSound [1.0, 0.9, 0.8, 0.4])
      asFloatSound (floatSound [0.1, 0.2, 0.3] <+> floatSound [0.9, 0.7, 0.5, 0.3]) `shouldBe` (floatSound [1.0, 0.9, 0.8, 0.3])

test_asIntSound :: Spec
test_asIntSound = do
  describe "Types.asIntSound" $ do
    it "converting from Float to Int to Float should not change the content" $ do
      asFloatSound (asIntSound (floatSound [0.1, 0.2, 0.3])) `shouldBe` floatSound [0.1, 0.2, 0.3]
    it "converting from Int to Float to Int should not change the content" $ do
      asIntSound (asFloatSound (intSound [1, 2, 3])) `shouldBe` intSound [1, 2, 3]

sine, sine2 :: Instrument
sine = instrument $ \hz s -> map (sin . (* (hz * pi / 24000))) [0.0..48000 * s]
sine2 = instrument $ \hz s -> map (double . sin . (* (hz * pi / 24000))) [0.0..48000 * s]
double :: Float -> Float
double = (2*)

test_modifyInstrument :: Spec
test_modifyInstrument = do
  describe "Types.modifyInstrumnet" $ do
    it "should compose a modifier into an instrument" $ do
      arrange (modifyInstrument sine (modifier (map double))) 440 1 `shouldBe` arrange sine2 440 1
