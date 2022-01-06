module Day02Spec ( spec ) where

import SpecHelper

testInput = "2x3x4\n1x1x10"

spec :: Spec
spec = describe "Day 02" $ do
  it "Sample" $ do
    day02 testInput `shouldBe` ["101", "48"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day02.txt"
    day02 actualInput `shouldBe` ["1606483", "3842356"]