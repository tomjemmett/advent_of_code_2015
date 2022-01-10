module Day17Spec ( spec ) where

import SpecHelper

spec :: Spec
spec = describe "Day 17" $ do
  -- test case doesn't generalise
  it "Actual" $ do
    actualInput <- readFile "inputs/day17.txt"
    day17 actualInput `shouldBe` ["654", "57"]