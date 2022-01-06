module Day22Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 22" $ do
  it "Sample" $ do
    day22 testInput `shouldBe` ["", ""]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day22.txt"
    day22 actualInput `shouldBe` ["", ""]