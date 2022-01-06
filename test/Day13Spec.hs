module Day13Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 13" $ do
  it "Sample" $ do
    day13 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day13.txt"
    day13 actualInput `shouldBe` ["", ""]