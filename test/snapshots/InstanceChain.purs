-- @format --arrow-first
module InstanceChain where

data Foo k

instance semigroupFooBlah :: (Semigroup k)
                             => Semigroup (Foo k) where
  append = foo
else instance semigroupFooBlaz :: Semigroup (Foo k) where
  append x y = x
