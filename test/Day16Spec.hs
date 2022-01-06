module Day16Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 16" $ do
  it "Sample" $ do
    day16 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day16.txt"
    day16 actualInput `shouldBe` ["", ""]