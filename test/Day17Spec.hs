module Day17Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 17" $ do
  it "Sample" $ do
    day17 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day17.txt"
    day17 actualInput `shouldBe` ["", ""]