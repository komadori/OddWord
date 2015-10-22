{-# LANGUAGE Haskell2010, ScopedTypeVariables #-}

module Data.Word.Odd (
    -- * Odd Word Wrapper
    OddWord,

    -- * Type Numbers
    TypeNum,
    One,
    Zero,

    -- * Predefined Odd Words
    Word1, Word2, Word3, Word4, Word5, Word6, Word7,
    Word9,  Word10, Word11, Word12, Word13, Word14, Word15,
    Word17, Word18, Word19, Word20, Word21, Word22, Word23, Word24,
    Word25, Word26, Word27, Word28, Word29, Word30, Word31,
    Word33, Word34, Word35, Word36, Word37, Word38, Word39, Word40,
    Word41, Word42, Word43, Word44, Word45, Word46, Word47, Word48,
    Word49, Word50, Word51, Word52, Word53, Word54, Word55, Word56,
    Word57, Word58, Word59, Word60, Word61, Word62, Word63
) where

import Data.Bits
import Data.Word
import Data.Function

-- | OddWord wraps the integer type specified in the first type parameter and
-- exposes a subset of the available bits as an unsigned word. The number of
-- bits to expose is encoded into the second type parameter.
--
-- The predefined word types provided by this module give some examples.
newtype OddWord a n = OW {unOW :: a} deriving (Eq, Ord)

data TypeNumBuilder a = TypeNumBuilder Int Int

fromTypeNum :: TypeNumBuilder a -> Int
fromTypeNum (TypeNumBuilder x _) = x

-- | Intances of 'TypeNum' represent type-level numbers.
class TypeNum a where
    typeNum :: TypeNumBuilder a

-- | Represents a type-level number with a leading one bit followed by the
-- string of digits specified by a.
data One a

-- | Represents a type-level number with a placeholder zero bit followed by the
-- string of digits specified by a.
data Zero a

instance TypeNum () where
    typeNum = TypeNumBuilder 0 0

instance (TypeNum a) => TypeNum (One a) where
    typeNum = let (TypeNumBuilder n m) = (typeNum :: TypeNumBuilder a)
              in TypeNumBuilder (n+bit m) (m+1)

instance (TypeNum a) => TypeNum (Zero a) where
    typeNum = let (TypeNumBuilder n m) = (typeNum :: TypeNumBuilder a)
              in TypeNumBuilder (n) (m+1)

pairOW :: (a, a) -> (OddWord a n, OddWord a n)
pairOW = uncurry ((,) `on` OW)

owMask :: forall a n. (Num a, Bits a, TypeNum n) => OddWord a n
owMask = OW $ (flip (-) 1) $ bit $ fromTypeNum (typeNum :: TypeNumBuilder n)

maskOW :: forall a n. (Num a, Bits a, TypeNum n) => a -> OddWord a n
maskOW w = OW $ w .&. unOW (owMask :: OddWord a n)

mapFst :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFst f xs = map (\(a,c) -> (f a,c)) xs

instance (Show a) => Show (OddWord a n) where
    showsPrec p (OW x) s = showsPrec p x s
    show (OW x)          = show x
    showList xs          = showList $ map unOW xs 

instance (Read a, Num a, Bits a, TypeNum n) => Read (OddWord a n) where
    readsPrec p s = mapFst maskOW $ readsPrec p s
    readList s    = mapFst (map maskOW) $ readList s

instance (Num a, Bits a, TypeNum n) => Num (OddWord a n) where
    (OW l) + (OW r) = maskOW $ (l + r)
    (OW l) * (OW r) = maskOW $ (l * r)
    (OW l) - (OW r) = maskOW $ (l - r)
    negate (OW x)   = maskOW $ negate x
    abs w = w
    signum (OW x) | x == 0    = 0
                  | otherwise = 1
    fromInteger i = maskOW $ fromInteger i 

instance (Real a, Bits a, TypeNum n) => Real (OddWord a n) where
    toRational (OW x) = toRational x

instance (Num a, Bits a, TypeNum n) => Bounded (OddWord a n) where
    minBound = 0
    maxBound = owMask

instance (Enum a, Ord a, Num a, Bits a, TypeNum n) => Enum (OddWord a n) where
    succ x = x + 1
    pred x = x - 1
    toEnum i | i >= 0 && fromIntegral i <= unOW (owMask :: OddWord a n)
             = OW $ toEnum i
             | otherwise = error "OddWord: toEnum: Index out of bounds."
    fromEnum (OW x) = fromEnum x
    enumFrom x = enumFromTo x owMask
    enumFromThen x1 x2 = enumFromThenTo x1 x2 bound
                         where bound | x2 >= x1 = owMask
                                     | otherwise = 0
    enumFromTo (OW x) (OW y) = map OW $ enumFromTo x y
    enumFromThenTo (OW x1) (OW x2) (OW y) = map OW $ enumFromThenTo x1 x2 y

instance (Integral a, Bits a, TypeNum n) => Integral (OddWord a n) where
    quot (OW n) (OW d) = OW $ quot n d
    rem  (OW n) (OW d) = OW $ rem n d
    div  (OW n) (OW d) = OW $ div n d
    mod  (OW n) (OW d) = OW $ mod n d
    quotRem (OW n) (OW d) = pairOW $ quotRem n d
    divMod  (OW n) (OW d) = pairOW $ divMod n d
    toInteger (OW x) = toInteger x

instance (Num a, Bits a, TypeNum n) => Bits (OddWord a n) where
    (OW l) .&. (OW r) = OW $ l .&. r
    (OW l) .|. (OW r) = OW $ l .|. r
    xor (OW l) (OW r) = OW $ xor l r
    complement (OW x) = maskOW $ complement x
    bit n | n < fromTypeNum (typeNum :: TypeNumBuilder n)
          = OW $ bit n
          | otherwise = OW 0
    setBit (OW x) n | n < fromTypeNum (typeNum :: TypeNumBuilder n)
                    = OW $ setBit x n
                    | otherwise = OW x
    clearBit (OW x) n = OW $ clearBit x n
    complementBit (OW x) n | n < fromTypeNum (typeNum :: TypeNumBuilder n)
                           = OW $ complementBit x n
                           | otherwise = OW x
    testBit (OW x) n = testBit x n
    bitSize _  = fromTypeNum (typeNum :: TypeNumBuilder n)
    isSigned _ = False 
    shiftL (OW x) n = maskOW $ shiftL x n
    shiftR (OW x) n = OW $ shiftR x n
    rotateL (OW x) n = OW $
        (shiftL x n' .&. unOW (owMask :: OddWord a n)) .|. shiftR x (w-n')
        where n' = n `mod` w
              w = fromTypeNum (typeNum :: TypeNumBuilder n)
    rotateR (OW x) n = OW $
        shiftR x n' .|. (shiftL x (w-n') .&. unOW (owMask :: OddWord a n))
        where n' = n `mod` w
              w  = fromTypeNum (typeNum :: TypeNumBuilder n)
    popCount (OW x) = popCount x

type Word1  = OddWord Word8             (One  ())
type Word2  = OddWord Word8        (One (Zero ()))
type Word3  = OddWord Word8        (One (One  ()))
type Word4  = OddWord Word8  (One (Zero (Zero ())))
type Word5  = OddWord Word8  (One (Zero (One  ())))
type Word6  = OddWord Word8  (One (One  (Zero ())))
type Word7  = OddWord Word8  (One (One  (One  ())))
--type Word8
type Word9  = OddWord Word16 (One (Zero (Zero (One  ()))))
type Word10 = OddWord Word16 (One (Zero (One  (Zero ()))))
type Word11 = OddWord Word16 (One (Zero (One  (One  ()))))
type Word12 = OddWord Word16 (One (One  (Zero (Zero ()))))
type Word13 = OddWord Word16 (One (One  (Zero (One  ()))))
type Word14 = OddWord Word16 (One (One  (One  (Zero ()))))
type Word15 = OddWord Word16 (One (One  (One  (One  ()))))
--type Word16
type Word17 = OddWord Word32 (One (Zero (Zero (Zero (One  ())))))
type Word18 = OddWord Word32 (One (Zero (Zero (One  (Zero ())))))
type Word19 = OddWord Word32 (One (Zero (Zero (One  (One  ())))))
type Word20 = OddWord Word32 (One (Zero (One  (Zero (Zero ())))))
type Word21 = OddWord Word32 (One (Zero (One  (Zero (One  ())))))
type Word22 = OddWord Word32 (One (Zero (One  (One  (Zero ())))))
type Word23 = OddWord Word32 (One (Zero (One  (One  (One  ())))))
type Word24 = OddWord Word32 (One (One  (Zero (Zero (Zero ())))))
type Word25 = OddWord Word32 (One (One  (Zero (Zero (One  ())))))
type Word26 = OddWord Word32 (One (One  (Zero (One  (Zero ())))))
type Word27 = OddWord Word32 (One (One  (Zero (One  (One  ())))))
type Word28 = OddWord Word32 (One (One  (One  (Zero (Zero ())))))
type Word29 = OddWord Word32 (One (One  (One  (Zero (One  ())))))
type Word30 = OddWord Word32 (One (One  (One  (One  (Zero ())))))
type Word31 = OddWord Word32 (One (One  (One  (One  (One  ())))))
--type Word32
type Word33 = OddWord Word64 (One (Zero (Zero (Zero (Zero (One  ()))))))
type Word34 = OddWord Word64 (One (Zero (Zero (Zero (One  (Zero ()))))))
type Word35 = OddWord Word64 (One (Zero (Zero (Zero (One  (One  ()))))))
type Word36 = OddWord Word64 (One (Zero (Zero (One  (Zero (Zero ()))))))
type Word37 = OddWord Word64 (One (Zero (Zero (One  (Zero (One  ()))))))
type Word38 = OddWord Word64 (One (Zero (Zero (One  (One  (Zero ()))))))
type Word39 = OddWord Word64 (One (Zero (Zero (One  (One  (One  ()))))))
type Word40 = OddWord Word64 (One (Zero (One  (Zero (Zero (Zero ()))))))
type Word41 = OddWord Word64 (One (Zero (One  (Zero (Zero (One  ()))))))
type Word42 = OddWord Word64 (One (Zero (One  (Zero (One  (Zero ()))))))
type Word43 = OddWord Word64 (One (Zero (One  (Zero (One  (One  ()))))))
type Word44 = OddWord Word64 (One (Zero (One  (One  (Zero (Zero ()))))))
type Word45 = OddWord Word64 (One (Zero (One  (One  (Zero (One  ()))))))
type Word46 = OddWord Word64 (One (Zero (One  (One  (One  (Zero ()))))))
type Word47 = OddWord Word64 (One (Zero (One  (One  (One  (One  ()))))))
type Word48 = OddWord Word64 (One (One  (Zero (Zero (Zero (Zero ()))))))
type Word49 = OddWord Word64 (One (One  (Zero (Zero (Zero (One  ()))))))
type Word50 = OddWord Word64 (One (One  (Zero (Zero (One  (Zero ()))))))
type Word51 = OddWord Word64 (One (One  (Zero (Zero (One  (One  ()))))))
type Word52 = OddWord Word64 (One (One  (Zero (One  (Zero (Zero ()))))))
type Word53 = OddWord Word64 (One (One  (Zero (One  (Zero (One  ()))))))
type Word54 = OddWord Word64 (One (One  (Zero (One  (One  (Zero ()))))))
type Word55 = OddWord Word64 (One (One  (Zero (One  (One  (One  ()))))))
type Word56 = OddWord Word64 (One (One  (One  (Zero (Zero (Zero ()))))))
type Word57 = OddWord Word64 (One (One  (One  (Zero (Zero (One  ()))))))
type Word58 = OddWord Word64 (One (One  (One  (Zero (One  (Zero ()))))))
type Word59 = OddWord Word64 (One (One  (One  (Zero (One  (One  ()))))))
type Word60 = OddWord Word64 (One (One  (One  (One  (Zero (Zero ()))))))
type Word61 = OddWord Word64 (One (One  (One  (One  (Zero (One  ()))))))
type Word62 = OddWord Word64 (One (One  (One  (One  (One  (Zero ()))))))
type Word63 = OddWord Word64 (One (One  (One  (One  (One  (One  ()))))))
