module Guards where

test
  | a = b
  | otherwise = c

test | a = b
     | otherwise = c

test | a = b | otherwise = c

test | a =
  b
  | otherwise = c

test | a =
  b | otherwise =
  c

test = case _ of
  a
    | a -> b
    | otherwise -> c

test = case _ of
  a | a -> b
    | otherwise -> c

test = case _ of
  a | a -> b | otherwise -> c

test = case _ of
  a | a ->
    b | otherwise ->
    c

test = case _ of
  a | a ->
    b
    | otherwise ->
    c
