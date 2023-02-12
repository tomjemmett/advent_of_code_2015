module Day25Spec (spec) where

import SpecHelper

testInput = "(6, 6)"

spec :: Spec
spec = describe "Day 25" $ do
  it "Sample" $ do
    day25 testInput `shouldBe` ["27995004"]

  it "Actual" $ do
    actualInput <- readFile "inputs/day25.txt"
    day25 actualInput `shouldBe` ["8997277"]