module Day12Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    day12 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day12.txt"
    day12 actualInput `shouldBe` ["", ""]