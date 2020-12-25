{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "halogen-storybook"
  , "psci-support"
  , "purescript-halogen-animations"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
