module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Hooks.Animations (animated)
import Halogen.Storybook (Stories, runStorybook)

stories ∷ Stories Aff
stories = Object.fromFoldable
  [ Tuple "animated" (animated { onStart: "on-start", toFinal: "to-final", onFinal: "on-final", toStart: "to-start" })
  ]

main :: Effect Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runStorybook { stories, logo: Just $ HH.text "Halogen Animations" } body
