module Day01Spec ( spec ) where

import SpecHelper

testInput = "()())"

spec :: Spec
spec = describe "Day 01" $ do
  it "Sample" $ do
    day01 testInput `shouldBe` ["-1", "5"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day01.txt"
    day01 actualInput `shouldBe` ["138", "1771"]