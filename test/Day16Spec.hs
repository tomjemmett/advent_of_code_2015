module Day16Spec ( spec ) where

import SpecHelper

spec :: Spec
spec = describe "Day 16" $ do
  -- no sample

  it "Actual" $ do
    actualInput <- readFile "inputs/day16.txt"
    day16 actualInput `shouldBe` ["40", "241"]