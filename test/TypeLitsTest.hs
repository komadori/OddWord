{-# LANGUAGE Haskell2010, ScopedTypeVariables, DataKinds #-}

module Main where

import System.Exit
import Data.Bits
import Data.Word
import Data.Word.Odd
import GHC.TypeLits

typeLitWordLengths :: [Int]
typeLitWordLengths = [
    finiteBitSize (0 :: OddWord Word8 (Lit 1)),
    finiteBitSize (0 :: OddWord Word8 (Lit 2)),
    finiteBitSize (0 :: OddWord Word8 (Lit 3)),
    finiteBitSize (0 :: OddWord Word8 (Lit 4))]

main :: IO ()
main = do
    -- Verify lengths of odd words defined using GHC type-level literals
    mapM_ (\(u,v) -> putStrLn (
        showString "Error: Word" . shows u . showString " has length " $
            shows v ".") >> exitFailure) $
        map fst $ filter snd $ map (\t -> (t,uncurry (/=) t)) $
        zip [1..] typeLitWordLengths
