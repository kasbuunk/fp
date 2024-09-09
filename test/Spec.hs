module Main (main) where

import qualified Chapter1Spec
import qualified Chapter2Spec
import qualified Chapter3Spec
import qualified Chapter4Spec
import qualified Chapter5Spec
import qualified Chapter6Spec
import qualified PlaygroundSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Playground" PlaygroundSpec.spec
  describe "Chapter1" Chapter1Spec.spec
  describe "Chapter2" Chapter2Spec.spec
  describe "Chapter3" Chapter3Spec.spec
  describe "Chapter4" Chapter4Spec.spec
  describe "Chapter5" Chapter5Spec.spec
  describe "Chapter6" Chapter6Spec.spec
