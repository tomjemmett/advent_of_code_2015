module Day21Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 21" $ do
  it "Actual" $ do
    actualInput <- readFile "inputs/day21.txt"
    day21 actualInput `shouldBe` ["111", "188"]