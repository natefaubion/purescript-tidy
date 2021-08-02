module Test.Main where

import Prelude

import Ansi.Output (foreground, withGraphics)
import Data.Array as Array
import Data.Foldable (any, findMap, fold, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Dodo.Ansi (Color(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Node.Process as Process
import Test.Snapshot (SnapshotResult(..), SnapshotResultGroup(..), snapshotFormat)

main :: Effect Unit
main = do
  args <- Process.argv
  let accept = any (eq "--accept" || eq "-a") args
  let printOutput = any (eq "--print-output" || eq "-p") args
  let filter = Pattern <$> findMap (String.stripPrefix (Pattern "--filter=")) args
  let printResult = printResultGroup printOutput
  launchAff_ do
    results@(SnapshotResultGroup { hasBad }) <- snapshotFormat accept filter
    printResult 0 results
    when hasBad do
      liftEffect $ Process.exit 1
  where
  indent :: Int -> String
  indent = fold <<< flip Array.replicate "  "

  printResultGroup :: Boolean -> Int -> SnapshotResultGroup -> Aff Unit
  printResultGroup printOutput lvl (SnapshotResultGroup { results, nested }) = do
    let
      logIndented :: Int -> String -> Aff Unit
      logIndented lvl' = Console.log <<< String.joinWith "\n" <<< map (append (indent lvl')) <<< String.split (Pattern "\n")

    for_ results \{ name, results: outputResults } -> do
      logIndented lvl $ "Checking " <> name
      for_ outputResults \{ output, result, directive } -> case result of
        Passed -> do
          logIndented (lvl + 1) $ withGraphics (foreground Green) "✓" <> " " <> directive <> " passed."
          when printOutput $ logIndented (lvl + 2) output
        Saved -> do
          logIndented (lvl + 1) $ withGraphics (foreground Yellow) "✓" <> " " <> directive <> " saved."
          when printOutput $ logIndented (lvl + 2) output
        Accepted -> do
          logIndented (lvl + 1) $ withGraphics (foreground Yellow) "✓" <> " " <> directive <> " accepted."
          when printOutput $ logIndented (lvl + 2) output
        Failed diff -> do
          logIndented (lvl + 1) $ withGraphics (foreground Red) "✗" <> " " <> directive <> " failed."
          logIndented (lvl + 2) diff
          when printOutput $ logIndented (lvl + 2) output
        ErrorRunningTest err -> do
          logIndented (lvl + 1) $ withGraphics (foreground Red) "✗" <> " " <> directive <> " failed due to an error."
          logIndented (lvl + 2) $ Error.message err

    for_ (Map.toUnfoldable nested :: Array (Tuple String SnapshotResultGroup)) \(Tuple path group) -> do
      logIndented lvl (correctCase path)
      printResultGroup printOutput (lvl + 1) group

  correctCase :: String -> String
  correctCase =
    String.split (Pattern "-")
      >>> map upperFirstChar
      >>> String.joinWith " "

  upperFirstChar :: String -> String
  upperFirstChar s = case Array.uncons (SCU.toCharArray s) of
    Nothing -> ""
    Just { head, tail } -> String.toUpper (SCU.fromCharArray (Array.singleton head)) <> SCU.fromCharArray tail
