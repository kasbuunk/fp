module Main (main) where 

import Test.Hspec
import qualified PlaygroundSpec

main :: IO ()
main = hspec $ do
    describe "Playground" PlaygroundSpec.spec
    -- Add the rest
