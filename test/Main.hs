{-# LANGUAGE Haskell2010, ScopedTypeVariables, CPP #-}

module Main where

import Prelude hiding (catch)
import System.Exit
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Data.Bits
import Data.Maybe
import Data.Word
import Data.Word.Odd
import Control.Applicative
import Control.Exception
import System.IO.Unsafe

-- | Represents a range of unary functions which can be applied to a word.
data UFunc
    = Add   Integer | Mul   Integer | Sub   Integer | SubR  Integer
    | Div   Integer | Mod   Integer | Quot  Integer | Rem   Integer
    | DivR  Integer | ModR  Integer | QuotR Integer | RemR  Integer
    | Neg           | Abs           | Inv           | AddDigit
    | From  Integer | And   Integer | Or    Integer | Xor   Integer
    | TstB  Int     | ClrB  Int     | SetB  Int     | InvB  Int
    | FromB Int     | Shift Int     | Rot   Int     | PopCnt
#if MIN_VERSION_base(4,8,0)
    | CntLZ         | CntTZ
#endif
    | AdjEnum Int Integer
    deriving Show

instance Arbitrary (UFunc) where
    arbitrary = oneof
        [Add   <$> choose (0, 0xffff)
        ,Mul   <$> choose (0, 0xffff)
        ,Sub   <$> choose (0, 0xffff)
        ,SubR  <$> choose (0, 0xffff)
        ,Div   <$> choose (0, 0xffff)
        ,Mod   <$> choose (0, 0xffff)
        ,Quot  <$> choose (0, 0xffff)
        ,Rem   <$> choose (0, 0xffff)
        ,DivR  <$> choose (0, 0xffff)
        ,ModR  <$> choose (0, 0xffff)
        ,QuotR <$> choose (0, 0xffff)
        ,RemR  <$> choose (0, 0xffff)
        ,return Neg
        ,return Abs
        ,return Inv
        ,return AddDigit
        ,From  <$> arbitrary
        ,And   <$> choose (0, 0xffff)
        ,Or    <$> choose (0, 0xffff)
        ,Xor   <$> choose (0, 0xffff)
        ,TstB  <$> choose (0, 32)
        ,ClrB  <$> choose (0, 32)
        ,SetB  <$> choose (0, 32)
        ,InvB  <$> choose (0, 32)
        ,FromB <$> choose (0, 32)
        ,Shift <$> choose (0, 32)
        ,Rot   <$> choose (0, 32)
        ,return PopCnt
#if MIN_VERSION_base(4,8,0)
        ,return CntLZ
        ,return CntTZ
#endif
        ,AdjEnum <$> choose (-0x1ffff, 0x1ffff) <*> choose (0, 0xffff)
        ]

-- | Total wrapper for 'div'.
safeDiv :: (Integral a, Bounded a) => a -> a -> a
safeDiv d 0 = maxBound
safeDiv d n = div d n

-- | Total wrapper for 'mod'.
safeMod :: (Integral a) => a -> a -> a
safeMod d 0 = 0
safeMod d n = mod d n

-- | Total wrapper for 'quot'.
safeQuot :: (Integral a, Bounded a) => a -> a -> a
safeQuot d 0 = maxBound
safeQuot d n = quot d n

-- | Total wrapper for 'rem'.
safeRem :: (Integral a) => a -> a -> a
safeRem d 0 = 0
safeRem d n = rem d n

-- | Total wrapper for 'toEnum'.
safeToEnum :: (Enum a) => a -> Int -> a
safeToEnum def x =
    unsafePerformIO (evaluate (toEnum x) `catch` \(ErrorCall _) -> return def)

-- | Interpreter for executing 'UFunc' values.
#if MIN_VERSION_base(4,7,0)
fromUFunc :: (Integral a, Bounded a, Enum a, FiniteBits a, Read a, Show a) =>
#else
fromUFunc :: (Integral a, Bounded a, Enum a, Bits a, Read a, Show a) =>
#endif
    UFunc -> a -> a
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
fromUFunc (From i)  _ = fromInteger i
fromUFunc  AddDigit x = read . ('1':) $ show x
fromUFunc (And   i) x = x .&. (fromInteger i)
fromUFunc (Or    i) x = x .|. (fromInteger i)
fromUFunc (Xor   i) x = xor x (fromInteger i)
fromUFunc (TstB  n) x = fromIntegral $ fromEnum $ testBit x n
fromUFunc (ClrB  n) x = clearBit x n
fromUFunc (SetB  n) x = setBit x n
fromUFunc (InvB  n) x = complementBit x n
fromUFunc (FromB n) _ = bit n
fromUFunc (Shift n) x = shift x n
fromUFunc (Rot   n) x = rotate x n
fromUFunc  PopCnt   x = fromIntegral $ popCount x
#if MIN_VERSION_base(4,8,0)
fromUFunc  CntLZ    x = fromIntegral $ countLeadingZeros x
fromUFunc  CntTZ    x = fromIntegral $ countTrailingZeros x
#endif
fromUFunc (AdjEnum i def) x = safeToEnum (fromIntegral def) . (+i) $ fromEnum x

-- | A 16-bit word underlied by a 64-bit word.
type TestWord16 = OddWord Word64 (One (Zero (Zero (Zero (Zero ())))))

-- | Checks that computations using real and simulated 16-bit words produce
-- the same result for a series of 'UFunc's.
verifyTestWord16 :: [UFunc] -> Bool
verifyTestWord16 us =
    let refFn = foldr (.) id $ map fromUFunc us :: Word16 -> Word16
        tstFn = foldr (.) id $ map fromUFunc us :: TestWord16 -> TestWord16
    in toInteger (refFn 0) == toInteger (tstFn 0)

-- | List of bit lengths for all words up to 63-bits.
preDefWordLengths :: [Int]
preDefWordLengths = [
    bits (0 :: Word1), bits (0 :: Word2), bits (0 :: Word3),
    bits (0 :: Word4), bits (0 :: Word5), bits (0 :: Word6),
    bits (0 :: Word7), bits (0 :: Word8), bits (0 :: Word9),
    bits (0 :: Word10), bits (0 :: Word11), bits (0 :: Word12),
    bits (0 :: Word13), bits (0 :: Word14), bits (0 :: Word15),
    bits (0 :: Word16), bits (0 :: Word17), bits (0 :: Word18),
    bits (0 :: Word19), bits (0 :: Word20), bits (0 :: Word21),
    bits (0 :: Word22), bits (0 :: Word23), bits (0 :: Word24),
    bits (0 :: Word25), bits (0 :: Word26), bits (0 :: Word27),
    bits (0 :: Word28), bits (0 :: Word29), bits (0 :: Word30),
    bits (0 :: Word31), bits (0 :: Word32), bits (0 :: Word33),
    bits (0 :: Word34), bits (0 :: Word35), bits (0 :: Word36),
    bits (0 :: Word37), bits (0 :: Word38), bits (0 :: Word39),
    bits (0 :: Word40), bits (0 :: Word41), bits (0 :: Word42),
    bits (0 :: Word43), bits (0 :: Word44), bits (0 :: Word45),
    bits (0 :: Word46), bits (0 :: Word47), bits (0 :: Word48),
    bits (0 :: Word49), bits (0 :: Word50), bits (0 :: Word51),
    bits (0 :: Word52), bits (0 :: Word53), bits (0 :: Word54),
    bits (0 :: Word55), bits (0 :: Word56), bits (0 :: Word57),
    bits (0 :: Word58), bits (0 :: Word59), bits (0 :: Word60),
    bits (0 :: Word61), bits (0 :: Word62), bits (0 :: Word63)]
#if MIN_VERSION_base(4,8,0)
    where bits n = fromMaybe 0 $ bitSizeMaybe n
#else
    where bits n = bitSize n
#endif

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
