module Day19Spec ( spec ) where

import SpecHelper

spec :: Spec
spec = describe "Day 19" $ do
  it "Actual" $ do
    actualInput <- readFile "inputs/day19.txt"
    day19 actualInput `shouldBe` ["509", "195"]