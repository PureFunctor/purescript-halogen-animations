module Example where

import Prelude

import Halogen as H
import Halogen.HTML as HH


component =
  H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval
  }
  where
  render _ = HH.div [ ] [ HH.text "Hello, World" ]
