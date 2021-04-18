{ name = "purescript"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "free"
  , "integers"
  , "lazy"
  , "lists"
  , "math"
  , "maybe"
  , "ordered-collections"
  , "pha"
  , "prelude"
  , "profunctor-lenses"
  , "random"
  , "record"
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
