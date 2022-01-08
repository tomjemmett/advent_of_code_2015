module Day10Spec ( spec ) where

import SpecHelper

testInput = "1"

spec :: Spec
spec = describe "Day 10" $ do
  it "Sample" $ do
    day10 testInput `shouldBe` ["82350", "1166642"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day10.txt"
    day10 actualInput `shouldBe` ["492982", "6989950"]