-- @format --width=10
module TrailingLineComments where

test -- ok
  = test

test = test -- ok

test = test {- a -} {- b -} -- ok
