module Day23Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 23" $ do
  it "Sample" $ do
    day23 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day23.txt"
    day23 actualInput `shouldBe` ["", ""]