{-# LANGUAGE Haskell2010, ScopedTypeVariables, DataKinds #-}

module Props where

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Gen
import Data.Bits
import Data.Proxy
import Data.Word
import Data.Word.Odd
import GHC.TypeLits

import Equiv

genOddWord :: forall a n.
    (Integral a, Bits a, TypeNum n) =>
    Proxy (OddWord a n) -> Gen (OddWord a n)
genOddWord _ =
    fmap (fromIntegral :: Integer -> OddWord a n) $ choose (
        fromIntegral (minBound::OddWord a n),
        fromIntegral (maxBound::OddWord a n))

propRotateRL :: (Integral a, FiniteBitsBase a, TypeNum n) =>
    Proxy (OddWord a n) -> Property
propRotateRL proxy =
    property $ do
        x <- genOddWord proxy
        i <- choose (0, 2*finiteBitSize x)
        return $ (==) x $ flip rotateL i $ rotateR x i

propInBounds :: forall a n nn.
    (TypeNum n, KnownNat nn,
     Integral a, Bounded a, Enum a, FiniteBitsBase a, Read a, Show a) =>
    Proxy nn -> Proxy (OddWord a n) -> Property
propInBounds _ _ = property $ \(us :: [UFunc nn]) ->
    let tstFn = foldr (.) id $ map fromUFunc us :: OddWord a n -> OddWord a n
        value = toInteger $ tstFn 0
    in value >= toInteger (minBound :: OddWord a n) &&
       value <= toInteger (maxBound :: OddWord a n)
