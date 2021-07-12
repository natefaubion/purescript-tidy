module Test.Main where

import Prelude

import Ansi.Output (foreground, withGraphics)
import Data.Foldable (any, findMap, for_)
import Data.String (Pattern(..))
import Data.String as String
import Dodo.Ansi (Color(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Node.Process as Process
import Test.Snapshot (SnapshotResult(..), isBad, snapshotFormat)

main :: Effect Unit
main = do
  args <- Process.argv
  let accept = any (eq "--accept" || eq "-a") args
  let printOutput = any (eq "--print-output" || eq "-p") args
  let filter = Pattern <$> findMap (String.stripPrefix (Pattern "--filter=")) args
  launchAff_ do
    results <- snapshotFormat "./test/snapshots" accept filter
    for_ results \{ name, results: outputResults } -> do
      Console.log $ "Checking " <> name
      for_ outputResults \{ output, result, directive } -> case result of
        Passed -> do
          Console.log $ withGraphics (foreground Green) "  ✓" <> " " <> directive <> " passed."
          when printOutput $ Console.log output
        Saved -> do
          Console.log $ withGraphics (foreground Yellow) "  ✓" <> " " <> directive <> " saved."
          when printOutput $ Console.log output
        Accepted -> do
          Console.log $ withGraphics (foreground Yellow) "  ✓" <> " " <> directive <> " accepted."
          when printOutput $ Console.log output
        Failed diff -> do
          Console.log $ withGraphics (foreground Red) "  ✗" <> " " <> directive <> " failed."
          Console.log diff
          when printOutput $ Console.log output
        ErrorRunningTest err -> do
          Console.log $ withGraphics (foreground Red) "  ✗" <> " " <> directive <> " failed due to an error."
          Console.log $ Error.message err
    when (any (any (isBad <<< _.result) <<< _.results) results) do
      liftEffect $ Process.exit 1
