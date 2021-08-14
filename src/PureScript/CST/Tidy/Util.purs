module Tidy.Util where

import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Unsafe (unsafeRegex)

splitLines :: String -> Array String
splitLines = Regex.split (unsafeRegex """\r?\n""" Flags.global)

splitStringEscapeLines :: String -> Array String
splitStringEscapeLines = Regex.split (unsafeRegex """\\ *\r?\n\s*\\""" Flags.global)