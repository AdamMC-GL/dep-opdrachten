module Main (main) where

import Test.Hspec
import Types
import Instruments

main :: IO ()
main = mapM_ hspec [test_silence, test_defaultSquare, test_defaultTriangle, test_pad]

test_silence :: Spec
test_silence = do
  describe "Instruments.silence" $ do
    it "generates silence" $ do
      asFloatSound (silence 0.001) `shouldBe` floatSound (replicate 49 0.0)

test_defaultSquare :: Spec
test_defaultSquare = do
  describe "Instruments.defaultSquare" $ do
    it "generates a square wave modified by attack and release" $ do
      asFloatSound (arrange defaultSquare 440 0.001) `shouldBe` floatSound [0.0,4.7000005e-5,9.200001e-5,1.3500001e-4,1.7600002e-4,2.1500002e-4,2.5200003e-4,2.87e-4,3.2000005e-4,3.5100002e-4,3.8000004e-4,4.0700004e-4,4.3200003e-4,4.55e-4,4.7600004e-4,4.9500004e-4,5.1200006e-4,5.2700005e-4,5.4000004e-4,5.5100006e-4,5.600001e-4,5.6700007e-4,5.7200005e-4,5.750001e-4,5.76e-4,5.750001e-4,5.7200005e-4,5.6700007e-4,-5.600001e-4,-5.5100006e-4,-5.4000004e-4,-5.2700005e-4,-5.1200006e-4,-4.9500004e-4,-4.7600004e-4,-4.55e-4,-4.3200003e-4,-4.0700004e-4,-3.8000004e-4,-3.5100002e-4,-3.2000005e-4,-2.87e-4,-2.5200003e-4,-2.1500002e-4,-1.7600002e-4,-1.3500001e-4,-9.200001e-5,-4.7000005e-5,-0.0]

test_defaultTriangle :: Spec
test_defaultTriangle = do
  describe "Instruments.defaultTriangle" $ do
    it "generates a triangle wave modified by attack and release" $ do
      asFloatSound (arrange defaultTriangle 440 0.001) `shouldBe` floatSound [0.0,2.7070062e-6,1.0597641e-5,2.3326325e-5,4.05475e-5,6.191556e-5,8.708497e-5,1.1571009e-4,1.4744543e-4,1.8194536e-4,2.188643e-4,2.578567e-4,2.9857698e-4,3.4067954e-4,3.838189e-4,4.276493e-4,4.7182542e-4,5.1600114e-4,5.201682e-4,4.990288e-4,4.7492626e-4,4.4820606e-4,4.1921367e-4,3.8829466e-4,3.557947e-4,3.2205935e-4,2.874343e-4,2.522648e-4,2.168968e-4,1.8167557e-4,1.4694694e-4,1.1305627e-4,8.034929e-5,4.917151e-5,1.9868454e-5,-7.214186e-6,-3.173096e-5,-5.3336174e-5,-7.1684364e-5,-8.642989e-5,-9.722714e-5,-1.0373064e-4,-1.0559466e-4,-1.0247381e-4,-9.4022405e-5,-7.989492e-5,-5.9745726e-5,-3.322928e-5,-0.0]

test_pad :: Spec
test_pad = do
  describe "Instruments.pad" $ do
    it "pads a given range of notes with a quarter pause, both left and right" $ do
      pad [Note C Four Quarter] `shouldBe` [Pause Quarter, Note C Four Quarter, Pause Quarter]
      pad [Note C Four Quarter, Note C Four Quarter] `shouldBe` [Pause Quarter, Note C Four Quarter, Note C Four Quarter, Pause Quarter]
