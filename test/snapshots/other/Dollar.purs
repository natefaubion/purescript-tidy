module Dollar where

foo = a $ b $
  c $
  d

foo = a $ b $
  -- ok
  ( if x then
      y
    else
      z
  )
  $ case _ of
      Ok -> true


  $ c
  $
    a
    b c
  -- oh
  $ [ a
  , b ]
