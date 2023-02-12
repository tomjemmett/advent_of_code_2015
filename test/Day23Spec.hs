module Day23Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 23" $ do
  it "Actual" $ do
    actualInput <- readFile "inputs/day23.txt"
    day23 actualInput `shouldBe` ["184", "231"]