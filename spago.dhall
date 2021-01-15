{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-halogen-animations"
, dependencies =
  [ "console", "effect", "free", "halogen", "psci-support", "typelevel-lists" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
