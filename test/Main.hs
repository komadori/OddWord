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
    Neg           | Abs           | Inv           | AddDigit      |
    And   Integer | Or    Integer | Xor   Integer |
    ClrB  Int     | SetB  Int     | InvB  Int     |
    Shift Int     | Rot   Int     | PopCnt
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
        return AddDigit,
        choose (0, 0xffff) >>= return . And,
        choose (0, 0xffff) >>= return . Or,
        choose (0, 0xffff) >>= return . Xor,
        choose (0, 32) >>= return . ClrB,
        choose (0, 32) >>= return . SetB,
        choose (0, 32) >>= return . InvB,
        choose (-32, 32) >>= return . Shift,
        choose (-32, 32) >>= return . Rot,
        return PopCnt]

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

fromUFunc :: (Integral a, Bounded a, Bits a, Read a, Show a) => UFunc -> a -> a
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
fromUFunc  AddDigit x = read . ('1':) $ show x
fromUFunc (And   i) x = x .&. (fromInteger i)
fromUFunc (Or    i) x = x .|. (fromInteger i)
fromUFunc (Xor   i) x = xor x (fromInteger i)
fromUFunc (ClrB  n) x = clearBit x n
fromUFunc (SetB  n) x = setBit x n
fromUFunc (InvB  n) x = complementBit x n
fromUFunc (Shift n) x = shift x n
fromUFunc (Rot   n) x = rotate x n
fromUFunc  PopCnt   x = fromIntegral $ popCount x

type TestWord16 = OddWord Word32 (One (Zero (Zero (Zero (Zero ())))))

verifyTestWord16 :: [UFunc] -> Bool
verifyTestWord16 us =
    let refFn = foldr (.) id $ map fromUFunc us :: Word16 -> Word16
        tstFn = foldr (.) id $ map fromUFunc us :: TestWord16 -> TestWord16
    in toInteger (refFn 0) == toInteger (tstFn 0)

preDefWordLengths :: [Int]
preDefWordLengths = [
    bitSize (0 :: Word1), bitSize (0 :: Word2), bitSize (0 :: Word3),
    bitSize (0 :: Word4), bitSize (0 :: Word5), bitSize (0 :: Word6),
    bitSize (0 :: Word7), bitSize (0 :: Word8), bitSize (0 :: Word9),
    bitSize (0 :: Word10), bitSize (0 :: Word11), bitSize (0 :: Word12),
    bitSize (0 :: Word13), bitSize (0 :: Word14), bitSize (0 :: Word15),
    bitSize (0 :: Word16), bitSize (0 :: Word17), bitSize (0 :: Word18),
    bitSize (0 :: Word19), bitSize (0 :: Word20), bitSize (0 :: Word21),
    bitSize (0 :: Word22), bitSize (0 :: Word23), bitSize (0 :: Word24),
    bitSize (0 :: Word25), bitSize (0 :: Word26), bitSize (0 :: Word27),
    bitSize (0 :: Word28), bitSize (0 :: Word29), bitSize (0 :: Word30),
    bitSize (0 :: Word31), bitSize (0 :: Word32), bitSize (0 :: Word33),
    bitSize (0 :: Word34), bitSize (0 :: Word35), bitSize (0 :: Word36),
    bitSize (0 :: Word37), bitSize (0 :: Word38), bitSize (0 :: Word39),
    bitSize (0 :: Word40), bitSize (0 :: Word41), bitSize (0 :: Word42),
    bitSize (0 :: Word43), bitSize (0 :: Word44), bitSize (0 :: Word45),
    bitSize (0 :: Word46), bitSize (0 :: Word47), bitSize (0 :: Word48),
    bitSize (0 :: Word49), bitSize (0 :: Word50), bitSize (0 :: Word51),
    bitSize (0 :: Word52), bitSize (0 :: Word53), bitSize (0 :: Word54),
    bitSize (0 :: Word55), bitSize (0 :: Word56), bitSize (0 :: Word57),
    bitSize (0 :: Word58), bitSize (0 :: Word59), bitSize (0 :: Word60),
    bitSize (0 :: Word61), bitSize (0 :: Word62), bitSize (0 :: Word63)]

main :: IO ()
main = do
    -- Verify lengths of predefined odd word synonyms
    mapM_ (\(u,v) -> putStrLn (
        showString "Error: Word" . shows u . showString " has length " $
            shows v ".") >> exitFailure) $
        map fst $ filter snd $ map (\t -> (t,uncurry (/=) t)) $
        zip [1..] preDefWordLengths
    -- Verify correctness of operations on a test word type
    r <- quickCheckWithResult stdArgs {maxSuccess = 1000} verifyTestWord16
    case r of
        Success _ _ _ -> exitSuccess
        _             -> exitFailure
