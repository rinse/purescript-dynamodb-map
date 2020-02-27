{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
    [ "aws-core"
    , "console"
    , "dynamodb-plumbing"
    , "effect"
    , "lists"
    , "map"
    , "ordered-collections"
    , "psci-support"
    , "transformers"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
