module Day14Spec ( spec ) where

import SpecHelper

testInput = "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
            \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

spec :: Spec
spec = describe "Day 14" $ do
  it "Sample" $ do
    day14 testInput `shouldBe` ["2640", "1564"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day14.txt"
    day14 actualInput `shouldBe` ["2548", "1102"]