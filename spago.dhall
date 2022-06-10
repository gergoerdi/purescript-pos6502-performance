{ name = "pos6502"
, dependencies =
  [ "arraybuffer"
  , "arraybuffer-types"
  , "console"
  , "effect"
  , "maybe"
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
