module Tidy.Util where

import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Unsafe (unsafeRegex)
import PureScript.CST.Types (Labeled(..), Name(..))

splitLines :: String -> Array String
splitLines = Regex.split (unsafeRegex """\r?\n""" Flags.global)

splitStringEscapeLines :: String -> Array String
splitStringEscapeLines = Regex.split (unsafeRegex """\\ *\r?\n\s*\\""" Flags.global)

nameOf :: forall a. Name a -> a
nameOf (Name { name }) = name

overLabel :: forall a b c. (a -> b) -> Labeled a c -> Labeled b c
overLabel k (Labeled lbl) = Labeled lbl { label = k lbl.label }
