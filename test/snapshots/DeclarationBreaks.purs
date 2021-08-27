module DeclarationBreaks where
import Foo
import Bar
type Ok = String
type OK2 :: Type
type Ok2 = String
test :: Int
test = 42
  where
  a = 1
  b = 2
  c :: Int
  c =
    let d :: Int
        d = 3
        e = 4
    in do
      let f = 5
          g :: Int
          g = 6
      g
