module Day18Spec ( spec ) where

import SpecHelper

spec :: Spec
spec = describe "Day 18" $ do
  it "Actual" $ do
    actualInput <- readFile "inputs/day18.txt"
    day18 actualInput `shouldBe` ["814", "924"]