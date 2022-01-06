module Day08Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 08" $ do
  it "Sample" $ do
    day08 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day08.txt"
    day08 actualInput `shouldBe` ["", ""]