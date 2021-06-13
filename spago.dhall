{ name = "argonaut-aeson-generic"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "record"
  , "test-unit"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
