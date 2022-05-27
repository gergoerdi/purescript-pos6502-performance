{ name = "pos6502"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "http-methods"
  , "lists"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "numerics"
  , "partial"
  , "pos6502"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "uint"
  , "word"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
