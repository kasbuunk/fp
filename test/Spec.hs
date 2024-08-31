module Main (main) where 

import Test.Hspec
import qualified PlaygroundSpec
import qualified Chapter1Spec
import qualified Chapter2Spec

main :: IO ()
main = hspec $ do
    describe "Playground" PlaygroundSpec.spec
    describe "Chapter1" Chapter1Spec.spec
    describe "Chapter2" Chapter2Spec.spec
    -- Add the rest
