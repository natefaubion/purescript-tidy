{ name = "purescript-tidy-script"
, dependencies =
  [ "effect"
  , "node-child-process"
  , "node-fs"
  , "node-process"
  ]
, packages = ../packages.dhall
, sources = [ "script/**/*.purs"  ]
}
