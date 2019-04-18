{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Foo where

class    (forall c. c ~ T a => Show (c b)) => ShowT a b
instance Show (T a b) => ShowT a b

class (forall b. Show b => ShowT a b) => C a where
  type family T a :: * -> *

data D a = MkD (T a Int)

instance C a => Show (D a) where
  show (MkD x) = show x
