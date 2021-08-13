module OperatorsReversed where

-- The last operand is allowed to break and indent without effecting
-- the rest of the operator chain.
test =
  a <> b <> c <>
    d

-- A break within the operator chain effects the rest of the chain.
test =
  a
    <> b <> c <>
    d

-- Operator last needs to be flipped back to operator first.
test =
  a <>
  b <>
  c <>
  d

-- Multiline delimiters need to retain a break after the operator.
test =
  a <>
  [ 1
    , 2, 3 ] <>
  [ 1
    , 2, 3 ]

-- Multiline hanging elements need to retain their multiline state.
test =
  a #
  ( \x ->
      x) #
  ( \x ->
      x)


-- For operator first, a break should be inserted after the operator.
-- Testing together because the rule for collapsing the break affects this.
test =
  a
    <> [ 1
    , 2, 3 ]
    <> [ 1
    , 2, 3 ]

test =
  a
    # ( \x ->
      x)
    # ( \x ->
      x)
