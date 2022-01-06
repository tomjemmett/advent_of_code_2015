module Day10Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    day10 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day10.txt"
    day10 actualInput `shouldBe` ["", ""]