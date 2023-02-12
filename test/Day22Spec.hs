module Day22Spec (spec) where

import SpecHelper

spec :: Spec
spec = describe "Day 22" $ do
  it "Actual" $ do
    actualInput <- readFile "inputs/day22.txt"
    day22 actualInput `shouldBe` ["953", "1289"]