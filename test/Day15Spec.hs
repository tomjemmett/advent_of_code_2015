module Day15Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    day15 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day15.txt"
    day15 actualInput `shouldBe` ["", ""]