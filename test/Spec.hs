module Main (main) where 

import Test.Hspec
import qualified Chapter1

main :: IO ()
main = hspec $ do
    describe "Chapter1" Chapter1.spec
    -- Add the rest
