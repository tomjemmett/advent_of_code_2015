module Day21Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 21" $ do
  it "Sample" $ do
    day21 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day21.txt"
    day21 actualInput `shouldBe` ["", ""]