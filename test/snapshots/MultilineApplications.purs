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
