module MultilineApplications where

import Prelude

test =
  do
    foo
  bar

test = do
    foo
  bar

test =
  foo
    # do
        foo
      bar

test =
  foo
    # do
        foo
      bar
    # do
        foo
      bar

test =
  do
    foo
  >>>
    bar

test = do
    foo
  >>>
    bar

test =
  foo
    # do
        foo
      >>>
        bar

test =
  case foo of
    Bar -> 42
  >>>
    bar

test = case foo of
    Bar -> 42
  >>>
    bar

test =
  case foo of
    Bar -> 42
  bar

test = case foo of
    Bar -> 42
  bar


test =
  foo bar
    # foo

test =
  foo
    bar do
      baz do
        42

test =
  foo
    bar do
      baz do
        42
    qux

test =
  foo
              bar do
                baz
            bar do
              baz
          bar do
            baz

test = foo bar \_ -> baz case _ of
    Foo -> 42

test = foo bar
  \_ -> baz case _ of
    Foo -> 42

test = foo
 bar
 \_ -> baz case _ of
    Foo -> 42

test =
 foo
 bar
 \_ -> baz case _ of
    Foo -> 42
