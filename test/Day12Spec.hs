module Day12Spec ( spec ) where

import SpecHelper

testInput = "[1,{\"c\":\"red\",\"b\":2},3]"

spec :: Spec
spec = describe "Day 12" $ do
  it "Sample" $ do
    day12 testInput `shouldBe` ["6", "4"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day12.txt"
    day12 actualInput `shouldBe` ["191164", "87842"]