{-# LANGUAGE Haskell2010, ScopedTypeVariables #-}

module Main where

import System.Exit
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Data.Bits
import Data.Word
import Data.Word.Odd
import Control.Monad

data UFunc =
    Add   Integer | Mul   Integer | Sub   Integer | SubR  Integer |
    Div   Integer | Mod   Integer | Quot  Integer | Rem   Integer |
    DivR  Integer | ModR  Integer | QuotR Integer | RemR  Integer |
    Neg           | Abs           | Inv           |
    And   Integer | Or    Integer | Xor   Integer |
    ClrB  Int     | SetB  Int     | InvB  Int     |
    Shift Int     | Rot   Int
    deriving Show

instance Arbitrary (UFunc) where
    arbitrary = oneof [
        choose (0, 0xffff) >>= return . Add,
        choose (0, 0xffff) >>= return . Mul,
        choose (0, 0xffff) >>= return . Sub,
        choose (0, 0xffff) >>= return . SubR,
        choose (0, 0xffff) >>= return . Div,
        choose (0, 0xffff) >>= return . Mod,
        choose (0, 0xffff) >>= return . Quot,
        choose (0, 0xffff) >>= return . Rem,
        choose (0, 0xffff) >>= return . DivR,
        choose (0, 0xffff) >>= return . ModR,
        choose (0, 0xffff) >>= return . QuotR,
        choose (0, 0xffff) >>= return . RemR,
        return Neg,
        return Abs,
        return Inv,
        choose (0, 0xffff) >>= return . And,
        choose (0, 0xffff) >>= return . Or,
        choose (0, 0xffff) >>= return . Xor,
        choose (0, 32) >>= return . ClrB,
        choose (0, 32) >>= return . SetB,
        choose (0, 32) >>= return . InvB,
        choose (-32, 32) >>= return . Shift,
        choose (-32, 32) >>= return . Rot]

safeDiv :: (Integral a, Bounded a) => a -> a -> a
safeDiv d 0 = maxBound
safeDiv d n = div d n

safeMod :: (Integral a) => a -> a -> a
safeMod d 0 = 0
safeMod d n = mod d n

safeQuot :: (Integral a, Bounded a) => a -> a -> a
safeQuot d 0 = maxBound
safeQuot d n = quot d n

safeRem :: (Integral a) => a -> a -> a
safeRem d 0 = 0
safeRem d n = rem d n

fromUFunc :: (Integral a, Bounded a, Bits a) => UFunc -> a -> a
fromUFunc (Add   i) x = x + (fromInteger i)
fromUFunc (Mul   i) x = x * (fromInteger i)
fromUFunc (Sub   i) x = x - (fromInteger i)
fromUFunc (SubR  i) x = (fromInteger i) - x
fromUFunc (Div   i) x = safeDiv  x (fromInteger i)
fromUFunc (Mod   i) x = safeMod  x (fromInteger i)
fromUFunc (Quot  i) x = safeQuot x (fromInteger i)
fromUFunc (Rem   i) x = safeRem  x (fromInteger i)
fromUFunc (DivR  i) x = safeDiv  (fromInteger i) x
fromUFunc (ModR  i) x = safeMod  (fromInteger i) x
fromUFunc (QuotR i) x = safeQuot (fromInteger i) x
fromUFunc (RemR  i) x = safeRem  (fromInteger i) x
fromUFunc  Neg      x = negate x
fromUFunc  Abs      x = abs x
fromUFunc  Inv      x = complement x
fromUFunc (And   i) x = x .&. (fromInteger i)
fromUFunc (Or    i) x = x .|. (fromInteger i)
fromUFunc (Xor   i) x = xor x (fromInteger i)
fromUFunc (ClrB  n) x = clearBit x n
fromUFunc (SetB  n) x = setBit x n
fromUFunc (InvB  n) x = complementBit x n
fromUFunc (Shift n) x = shift x n
fromUFunc (Rot   n) x = rotate x n

type TestWord16 = OddWord Word32 (One (Zero (Zero (Zero (Zero ())))))

verifyTestWord16 :: [UFunc] -> Bool
verifyTestWord16 us =
    let refFn = foldr (.) id $ map fromUFunc us :: Word16 -> Word16
        tstFn = foldr (.) id $ map fromUFunc us :: TestWord16 -> TestWord16
    in toInteger (refFn 0) == toInteger (tstFn 0)

main :: IO ()
main = do
    r <- quickCheckWithResult stdArgs {maxSuccess = 1000} verifyTestWord16
    case r of
        Success _ _ _ -> exitSuccess
        _             -> exitFailure
