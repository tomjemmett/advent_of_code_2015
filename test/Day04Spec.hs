module Day04Spec ( spec ) where

import SpecHelper

testInput = "abcdef"

spec :: Spec
spec = describe "Day 04" $ do
  it "Sample" $ do
    day04 testInput `shouldBe` ["609043", "6742839"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day04.txt"
    day04 actualInput `shouldBe` ["254575", "1038736"]