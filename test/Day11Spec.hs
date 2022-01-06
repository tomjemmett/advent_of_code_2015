module Day11Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 11" $ do
  it "Sample" $ do
    day11 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day11.txt"
    day11 actualInput `shouldBe` ["", ""]