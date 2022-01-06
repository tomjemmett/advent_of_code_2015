module Day19Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 19" $ do
  it "Sample" $ do
    day19 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day19.txt"
    day19 actualInput `shouldBe` ["", ""]