{-# LANGUAGE Haskell2010, DataKinds, KindSignatures, DeriveDataTypeable #-}

module Data.Word.Odd.TypeLits where

import Data.Typeable
import GHC.TypeLits

-- | Converts a native GHC type-level natural into one usable by this library.
-- This requires GHC 7.8 or later and the @DataKinds@ extension.
data Lit :: Nat -> * deriving Typeable
