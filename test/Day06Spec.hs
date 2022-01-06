module Day06Spec ( spec ) where

import SpecHelper

testInput = "toggle 0,0 through 999,999"

spec :: Spec
spec = describe "Day 06" $ do
  it "Sample" $ do
    day06 testInput `shouldBe` ["1000000","2000000"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day06.txt"
    day06 actualInput `shouldBe` ["569999", "17836115"]