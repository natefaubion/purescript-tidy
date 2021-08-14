module MultilineStringLiterals where

test = """
this is a string
"""

test =
  foo """
  this is a string
  """

test =
  foo
  """
  this is a string
  """

test =
  foo
  """           ok
                this is a string
  """

test =
  foo """         ok
                  this is a string
  """ bar

test =
  foo "           ok\
  \               this is a string\
  \" bar

test =
  foo "           ok\
            \               this is a string\
      \               this is a string\
\               this is a string\
          \" bar
