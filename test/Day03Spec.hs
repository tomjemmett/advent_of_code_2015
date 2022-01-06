module Day03Spec ( spec ) where

import SpecHelper

testInput = "^>v<"

spec :: Spec
spec = describe "Day 03" $ do
  it "Sample" $ do
    day03 testInput `shouldBe` ["4", "3"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day03.txt"
    day03 actualInput `shouldBe` ["2081", "2341"]