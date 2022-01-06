module Day18Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 18" $ do
  it "Sample" $ do
    day18 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day18.txt"
    day18 actualInput `shouldBe` ["", ""]