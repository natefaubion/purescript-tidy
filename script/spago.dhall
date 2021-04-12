{ name = "purescript-tidy-script"
, dependencies =
  [ "arrays"
  , "effect"
  , "maybe"
  , "node-buffer"
  , "node-child-process"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "prelude"
  , "strings"
  ]
, packages = ../packages.dhall
, sources = [ "script/**/*.purs"  ]
}
