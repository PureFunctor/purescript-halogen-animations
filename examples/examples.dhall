let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "debug"
        , "halogen-storybook"
        ]
  , sources =
      conf.sources #
        [ "examples/**/*.purs"
        ]
 }
