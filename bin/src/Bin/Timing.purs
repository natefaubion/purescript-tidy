module Bin.Timing where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)

type Timing =
  { seconds :: Number
  , nanos :: Number
  }

foreign import hrtime :: Effect Timing

foreign import hrtimeDiff :: Timing -> Effect Timing

toMilliseconds :: Timing -> Milliseconds
toMilliseconds { seconds, nanos } =
  Milliseconds $ seconds * 1000.0 + nanos / 1000000.0
