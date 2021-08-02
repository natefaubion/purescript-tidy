module Operator where

common =
  foo
    <> bar
    <> baz

flat =
  foo <>
  bar <>
  baz

mixed =
  yes && no
    || no && yes
    || yes && yes && no

parens =
  (a && b || c)
  || (a &&
      (b || (c && d)))
  && (a && (c && d))

functor =
  a <$> b
    <*> c
    <*> d

functorAlt =
  foo
    <$> bar <|> bar'
    <*> baz <|> baz'
    <*> qux

functorAltParens =
  foo
    <$> (bar <|> bar')
    <*> (baz <|> baz')
    <*> qux
