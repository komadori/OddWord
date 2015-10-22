{-# LANGUAGE Haskell2010, DataKinds, KindSignatures #-}

module Data.Word.Odd.TypeLits where

import GHC.TypeLits

-- | Converts a native GHC type-level natural into one usable by this library.
-- This requires GHC 7.8 or later and the @DataKinds@ extension.
data Lit :: Nat -> *
