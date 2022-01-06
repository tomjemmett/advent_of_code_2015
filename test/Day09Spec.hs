module Day09Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 09" $ do
  it "Sample" $ do
    day09 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day09.txt"
    day09 actualInput `shouldBe` ["", ""]