{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20211005/packages.dhall sha256:2ec351f17be14b3f6421fbba36f4f01d1681e5c7f46e0c981465c4cf222de5be

let additions =
    { tidy-codegen =
      { dependencies =
        [ "aff"
        , "ansi"
        , "arrays"
        , "avar"
        , "bifunctors"
        , "console"
        , "control"
        , "dodo-printer"
        , "effect"
        , "either"
        , "enums"
        , "exceptions"
        , "filterable"
        , "foldable-traversable"
        , "free"
        , "identity"
        , "integers"
        , "language-cst-parser"
        , "lazy"
        , "lists"
        , "maybe"
        , "newtype"
        , "node-buffer"
        , "node-child-process"
        , "node-fs-aff"
        , "node-path"
        , "node-process"
        , "node-streams"
        , "ordered-collections"
        , "parallel"
        , "partial"
        , "posix-types"
        , "prelude"
        , "record"
        , "safe-coerce"
        , "st"
        , "strings"
        , "tidy"
        , "transformers"
        , "tuples"
        , "type-equality"
        , "unicode"
        ]
      , repo = "https://github.com/natefaubion/purescript-tidy-codegen.git"
      , version = "v2.0.0"
      }
    , language-cst-parser =
         { dependencies =
           [ "arrays"
           , "const"
           , "effect"
           , "either"
           , "foldable-traversable"
           , "free"
           , "functors"
           , "maybe"
           , "numbers"
           , "ordered-collections"
           , "strings"
           , "transformers"
           , "tuples"
           , "typelevel-prelude"
           ]
         , repo = "https://github.com/natefaubion/purescript-language-cst-parser.git"
         , version = "v0.9.1"
         }
    , tidy =
    { dependencies =
      [ "arrays"
      , "dodo-printer"
      , "foldable-traversable"
      , "lists"
      , "maybe"
      , "ordered-collections"
      , "partial"
      , "prelude"
      , "language-cst-parser"
      , "strings"
      , "tuples"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy.git"
    , version = "v0.7.0"
    }
  }
in  upstream // additions
