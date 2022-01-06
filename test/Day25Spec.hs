module Day25Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 25" $ do
  it "Sample" $ do
    day25 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day25.txt"
    day25 actualInput `shouldBe` ["", ""]