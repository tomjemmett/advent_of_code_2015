module Day08Spec ( spec ) where

import SpecHelper

testInput = "\"\"\n\
            \\"abc\"\n\
            \\"aaa\\\"aaa\"\n\
            \\"\\x27\""


spec :: Spec
spec = describe "Day 08" $ do
  it "Sample" $ do
    day08 testInput `shouldBe` ["12", "19"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day08.txt"
    day08 actualInput `shouldBe` ["1371", "2117"]