module Day09Spec ( spec ) where

import SpecHelper

testInput = "London to Dublin = 464\n\
            \London to Belfast = 518\n\
            \Dublin to Belfast = 141"

spec :: Spec
spec = describe "Day 09" $ do
  it "Sample" $ do
    day09 testInput `shouldBe` ["605", "982"]
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day09.txt"
    day09 actualInput `shouldBe` ["251", "898"]