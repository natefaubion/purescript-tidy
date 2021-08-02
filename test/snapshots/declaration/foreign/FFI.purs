-- @format --arrow-first
module FFI where

foreign import kind OK

foreign import data Undefined :: Type -> Type

foreign import data
  UndefinedMultiline
  :: Type
  -> Type

foreign import undefined :: Undefined Int

foreign import
  undefinedMultiline
  :: forall a
   . Array a
   -> Undefined a

foreign import extremelyLongNamedValueThatHasManyTypes :: Int -> Int -> Int -> Int -> Int -> Int

foreign import mkEffectFn7 :: forall a b c d e f g r.
  (a -> b -> c -> d -> e -> f -> g -> Effect r) -> EffectFn7 a b c d e f g r
