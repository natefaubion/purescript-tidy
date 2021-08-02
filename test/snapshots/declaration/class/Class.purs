module Class where

class (Functor f
, Semigroup a) <= Foo (a :: Row Type)
  (b :: Type) k | a -> k a
  , b -> a where
  foo :: a
    -> b
