module Day24Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 24" $ do
  it "Sample" $ do
    day24 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day24.txt"
    day24 actualInput `shouldBe` ["", ""]