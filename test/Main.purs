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
    for_ results \{ name, output, result } -> case result of
      Passed -> do
        Console.log $ withGraphics (foreground Green) "✓" <> " " <> name <> " passed."
        when printOutput $ Console.log output
      Saved -> do
        Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " saved."
        when printOutput $ Console.log output
      Accepted -> do
        Console.log $ withGraphics (foreground Yellow) "✓" <> " " <> name <> " accepted."
        when printOutput $ Console.log output
      Failed diff -> do
        Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed."
        Console.log diff
        when printOutput $ Console.log output
      ErrorRunningTest err -> do
        Console.log $ withGraphics (foreground Red) "✗" <> " " <> name <> " failed due to an error."
        Console.log $ Error.message err
    when (any (isBad <<< _.result) results) do
      liftEffect $ Process.exit 1
