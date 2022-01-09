module Day15Spec ( spec ) where

import SpecHelper

testInput = "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\n\
            \Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3\n\
            \skip1: capacity 0, durability 0, flavor 0, texture 0, calories 0\n\
            \skip2: capacity 0, durability 0, flavor 0, texture 0, calories 0"

spec :: Spec
spec = describe "Day 15" $ do
  it "Sample" $ do
    day15 testInput `shouldBe` ["62842880", "57600000"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day15.txt"
    day15 actualInput `shouldBe` ["13882464", "11171160"]