module Main where

import Data.Word
import Data.Word.Odd
import Criterion.Main

testAddMul :: (Num a) => a -> a
testAddMul n = 2*n*n + 3*n + 4

main :: IO ()
main =
    defaultMain [
        bgroup "Word" [
            bench "addMul" $ whnf testAddMul (1::Word)
        ],
        bgroup "Word20" [
            bench "addMul" $ whnf testAddMul (1::Word20)
        ]
    ]
