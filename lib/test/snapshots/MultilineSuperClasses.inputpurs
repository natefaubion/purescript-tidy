module MultilineSuperClasses where

class (Foo m
  , Bar m) <= Baz m

class
  ( Foo m
  , Bar m
  ) <=
  Baz m

class (Foo m
  , Bar m) <= Baz m where
  foo :: m

class
  ( Foo m
  , Bar m
  ) <=
  Baz m
  where
  foo :: m

instance (Foo m
  , Bar m) => Baz m where
  foo = undefined

instance
  ( Foo m
  , Bar m
  ) =>
  Baz m where
  foo = undefined
