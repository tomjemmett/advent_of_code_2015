module Day24Spec (spec) where

import SpecHelper

testInput =
  "1\n\
  \2\n\
  \3\n\
  \4\n\
  \5\n\
  \7\n\
  \8\n\
  \9\n\
  \10\n\
  \11"

spec :: Spec
spec = describe "Day 24" $ do
  it "Sample" $ do
    day24 testInput `shouldBe` ["99", "44"]

  it "Actual" $ do
    actualInput <- readFile "inputs/day24.txt"
    day24 actualInput `shouldBe` ["10723906903", "74850409"]