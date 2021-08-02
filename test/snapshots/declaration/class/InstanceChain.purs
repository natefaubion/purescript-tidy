-- @format --arrow-first
module InstanceChain where

data Foo k

instance semigroupFooBlah :: (Semigroup k)
                             => Semigroup (Foo k) where
  append = foo
else instance semigroupFooBlaz :: Semigroup (Foo k) where
  append x y = x

instance foo :: Foo Bar where
  foo a b = a >< b
else instance
  foo :: (Functor f,
  RowCons a b c dd) =>
  Foo
  Bar where
  foo a b =
    a >< b
