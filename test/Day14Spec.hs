module Day14Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    day14 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day14.txt"
    day14 actualInput `shouldBe` ["", ""]