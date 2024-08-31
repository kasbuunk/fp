module Main (main) where 

import Test.Hspec
import qualified PlaygroundSpec
import qualified Chapter1Spec

main :: IO ()
main = hspec $ do
    describe "Playground" PlaygroundSpec.spec
    describe "Chapter1" Chapter1Spec.spec
    -- Add the rest
