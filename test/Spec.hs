module Main (main) where 

import Test.Hspec
import qualified Chapter1Spec

main :: IO ()
main = hspec $ do
    describe "Chapter1" Chapter1Spec.spec
    -- Add the rest
