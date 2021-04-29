{ name = "purescript"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "pha"
  , "prelude"
  , "profunctor-lenses"
  , "random"
  , "strings"
  , "tailrec"
  , "tuples"
  , "unfoldable"
  , "unsafe-coerce"
  , "web-events"
  , "web-html"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
