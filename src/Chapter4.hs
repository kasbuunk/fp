module Chapter4 where

even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0
