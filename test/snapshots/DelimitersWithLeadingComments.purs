module DelimitersWithLeadingComments where

test =
  -- Array
  [ a, b, c ]

test =
  -- Array
  [ a, b
  , c ]

test =
  -- Record
  { a, b, c }

test =
  -- Record
  { a, b
  , c }

test =
  -- Parens
  (a b c)

test =
  -- Parens
  (a b
  c)
