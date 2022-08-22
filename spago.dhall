{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "droplet"
, license = "MIT"
, repository = "https://github.com/easafe/purescript-droplet"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "bigints"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "integers"
  , "maybe"
  , "newtype"
  , "nonempty"
  , "nullable"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "record"
  , "spec"
  , "strings"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
