module Day05Spec ( spec ) where

import SpecHelper

testInput = "ugknbfddgicrmopn\n\
            \aaa\n\
            \jchzalrnumimnmhp\n\
            \haegwjzuvuyypxyu\n\
            \dvszwmarrgswjxmb\n\
            \qjhvhtzxzqqjkmpb\n\
            \xxyxx\n\
            \uurcxstgmygtbstg\n\
            \ieodomkazucvgmuy"

spec :: Spec
spec = describe "Day 05" $ do
  it "Sample" $ do
    day05 testInput `shouldBe` ["2", "2"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day05.txt"
    day05 actualInput `shouldBe` ["236", "51"]