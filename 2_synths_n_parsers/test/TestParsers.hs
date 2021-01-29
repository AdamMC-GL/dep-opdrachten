module Main (main) where

import Test.Hspec
import Types
import Parsers
import Control.Monad.Trans.State (evalStateT)

main :: IO ()
main = mapM_ hspec [test_pComplementCharSet, test_pString, test_pOptional, test_pRepeat, test_pNumber, test_pOctave, test_pHeader, test_parse]

test_pComplementCharSet :: Spec
test_pComplementCharSet = do
  describe "Parsers.pComplementCharSet" $ do
    it "parses any character not in the provided set" $ do
      evalStateT (pComplementCharSet ":-") "Yeet!" `shouldBe` Just 'Y'
      evalStateT (pComplementCharSet ":-") ";" `shouldBe` Just ';'
    it "does not parse any character in the provided set" $ do
      evalStateT (pComplementCharSet ":-") "-- comment" `shouldBe` Nothing
      evalStateT (pComplementCharSet ":-") "::" `shouldBe` Nothing

test_pString :: Spec
test_pString = do
  describe "Parsers.pString" $ do
    it "matches a given string and disregards the rest" $ do
      evalStateT (pString "foo bar") "foo bar" `shouldBe` Just "foo bar"
      evalStateT (pString "foo bar") "foo bar!" `shouldBe` Just "foo bar"
      evalStateT (pString "foo bar") "foo" `shouldBe` Nothing

test_pOptional :: Spec
test_pOptional = do
  describe "Parsers.pOptional" $ do
    it "applies the parser and wraps the result in Just" $ do
      evalStateT (pOptional $ pString "foo bar") "foo bar" `shouldBe` Just (Just "foo bar")
      evalStateT (pOptional $ pString "foo bar") "foo bar!" `shouldBe` Just (Just "foo bar")
    it "succeeds with Nothing if no parse is possible" $ do
      evalStateT (pOptional $ pString "foo bar") "foo" `shouldBe` Just Nothing

test_pRepeat :: Spec
test_pRepeat = do
  describe "Parsers.pRepeat" $ do
    it "repeats a given parser one or more times until failure" $ do
      evalStateT (pRepeat (pCharSet "x")) "xxxyyy" `shouldBe` Just "xxx"
    it "parses at least one time or fails" $ do
      evalStateT (pRepeat (pCharSet "x")) "yyy" `shouldBe` Nothing

test_pNumber :: Spec
test_pNumber = do
  describe "Parsers.pComplementCharSet" $ do
    it "parses a numerical string and converts to Int" $ do
      evalStateT pNumber "1234" `shouldBe` Just 1234
    it "does not parse a non-numerical string" $ do
      evalStateT pNumber "abcd" `shouldBe` Nothing
    it "parses a numerical prefix of a mixed string" $ do
      evalStateT pNumber "1234abcd" `shouldBe` Just 1234

test_pOctave :: Spec
test_pOctave = do
  describe "Parsers.pOctave" $ do
    it "parses a digit representing an Octave including conversion" $ do
      evalStateT pOctave "1" `shouldBe` Just One
    it "fails if no valid octave number is provided" $ do
      evalStateT pOctave "a" `shouldBe` Nothing

test_pHeader :: Spec
test_pHeader = do
  describe "Parsers.pHeader" $ do
    it "parses a RTTL header, including song title" $ do
      evalStateT pHeader "AbbaSOS:d=4, o=6, b=124:" `shouldBe`
        Just ("AbbaSOS", Quarter, Six, 124)

test_parse :: Spec
test_parse = do
  describe "Parsers.parse" $ do
    it "parses a ringtone" $ do
      parse "smario:d=4, o=6, b=100:p, 8c.5, 16g5, 8p, 8e5, 16p, 8a5, 16b5, 16p, 16a5, 8a5, 8g5, 8e, 8g, 8a, 16e, 16g, 16p, 8e, 16c, 16d, 8b.5, 8c., 16g5, 8p, 8e5, 16p, 8a5, 16b5, 16p, 16a5, 8a5, 8g5, 8e, 8g, 8a, 16f, 16g, 16p, 8e, 16c, 16d, 8b.5"
        `shouldBe` Just ("smario",
                         [ Pause Quarter, Note C Five (Dotted Eighth), Note G Five Sixteenth
                         , Pause Eighth, Note E Five Eighth, Pause Sixteenth, Note A Five Eighth
                         , Note B Five Sixteenth, Pause Sixteenth, Note A Five Sixteenth
                         , Note A Five Eighth, Note G Five Eighth, Note E Six Eighth
                         , Note G Six Eighth, Note A Six Eighth, Note E Six Sixteenth
                         , Note G Six Sixteenth, Pause Sixteenth, Note E Six Eighth
                         , Note C Six Sixteenth, Note D Six Sixteenth
                         , Note B Five (Dotted Eighth), Note C Six (Dotted Eighth)
                         , Note G Five Sixteenth, Pause Eighth, Note E Five Eighth
                         , Pause Sixteenth, Note A Five Eighth, Note B Five Sixteenth
                         , Pause Sixteenth, Note A Five Sixteenth, Note A Five Eighth
                         , Note G Five Eighth, Note E Six Eighth, Note G Six Eighth
                         , Note A Six Eighth, Note F Six Sixteenth, Note G Six Sixteenth
                         , Pause Sixteenth, Note E Six Eighth, Note C Six Sixteenth
                         , Note D Six Sixteenth, Note B Five (Dotted Eighth)], 100.0)
