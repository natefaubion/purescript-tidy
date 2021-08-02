-- @format --arrow-first
module KindSignature where

data T :: (k -> Type)
       -> k
       -> Type
data T m a = MkT (m a) (T Maybe (m a))

class C1 :: Type -> Constraint
class C1 a

type F
  :: Type -> Type
type F f = Int
