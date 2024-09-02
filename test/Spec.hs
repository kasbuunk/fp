module Main (main) where 

import Test.Hspec
import qualified PlaygroundSpec
import qualified Chapter1Spec
import qualified Chapter2Spec
import qualified Chapter3Spec
import qualified Chapter4Spec

main :: IO ()
main = hspec $ do
    describe "Playground" PlaygroundSpec.spec
    describe "Chapter1" Chapter1Spec.spec
    describe "Chapter2" Chapter2Spec.spec
    describe "Chapter3" Chapter3Spec.spec
    describe "Chapter4" Chapter4Spec.spec
    -- Add the rest
