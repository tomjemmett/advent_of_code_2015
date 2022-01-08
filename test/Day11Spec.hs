module Day11Spec ( spec ) where

import SpecHelper

spec :: Spec
spec = describe "Day 11" $ do
  -- no test case

  it "Actual" $ do
    actualInput <- readFile "inputs/day11.txt"
    day11 actualInput `shouldBe` ["cqjxxyzz", "cqkaabcc"]