module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Example as Example
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: forall m. Stories m
stories = Object.fromFoldable
  [ Tuple "" $ proxy Example.component
  ]

main = HA.runHalogenAff do
  HA.awaitBody >>=
    runStorybook
      { stories
      , logo: Nothing
      }
