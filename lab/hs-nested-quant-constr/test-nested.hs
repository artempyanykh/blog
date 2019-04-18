{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Foo where

data Pair a b = MkPair a b

class (forall c. c ~ Pair a => Show (c b)) => ShowPairlike a b
instance Show (Pair a b) => ShowPairlike a b

class (forall b. Show b => ShowPairlike a b) => ShowFst a

data PairWithInt a = MkPairWithInt (Pair a Int)

instance ShowFst a => Show (PairWithInt a) where
  show (MkPairWithInt x) = show x
