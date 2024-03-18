module VisibleTypeApplications where

test :: forall @a. a -> a

test :: forall @
  -- wat
  a. a -> a

test :: forall @{- wat -}a. a -> a

test :: forall (@a :: Type). a -> a

test :: forall (@
  -- wat
  a :: Type). a -> a

test :: forall (@{- wat -}a :: Type). a -> a

test = foo @Bar 42

test = foo @Bar
  42

test = foo
  @Bar
  42

test = foo @(
  Bar
    { baz :: Int
    , quux :: String
    }
) 42

test = foo @Bar 42 @(
  Baz
    { baz :: Int
    , quux :: String
    }
)
