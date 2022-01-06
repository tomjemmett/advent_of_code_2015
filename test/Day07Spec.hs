module Day07Spec ( spec ) where

import SpecHelper

testInput = ""

spec :: Spec
spec = describe "Day 07" $ do
  -- no given sample
      
  it "Actual" $ do
    actualInput <- readFile "inputs/day07.txt"
    day07 actualInput `shouldBe` ["46065", "14134"]