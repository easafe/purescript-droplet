{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "foreign"
  , "nullable"
  , "psci-support"
  , "record"
  , "strings"
  , "test-unit"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
