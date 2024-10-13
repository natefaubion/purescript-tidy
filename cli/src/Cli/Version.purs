module Cli.Version where

import Prelude
import Spago.Generated.BuildInfo (packages)

version :: String
version = "v" <> packages."tidy-cli"
