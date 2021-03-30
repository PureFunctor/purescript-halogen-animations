module Halogen.Hooks.Animations where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Web.Event.Event (Event, EventType(..))


-- | Record of CSS classes to be applied during each `Step`.
type Classes =
  { onStart ∷ String
  , toFinal ∷ String
  , onFinal ∷ String
  , toStart ∷ String
  }


-- | Enumerates animation steps.
data Step
  = OnStart
  | ToFinal
  | OnFinal
  | ToStart


-- | TODO: Remove
-- |
-- | Reference compoenent to be used for the package.
-- | This should be the basis for generalizing the
-- | implementation of the package.
animated ∷ ∀ q i o m. Classes → H.Component q i o m
animated classes = Hooks.component \_ _ → hooks
  where
  hooks = Hooks.do
    mStep /\ stepId ← Hooks.useState (Just OnStart)

    Hooks.pure do
      HH.div
        [ css $ case mStep of
             Just OnStart → classes.onStart
             Just ToFinal → classes.toFinal
             Just OnFinal → classes.onFinal
             Just ToStart → classes.toStart
             Nothing → ""
        , onAnimationEnd \_ → do
             case mStep of
               Just ToFinal → Hooks.modify_ stepId \_ → Just OnFinal
               Just ToStart → Hooks.modify_ stepId \_ → Just OnStart
               _ → pure unit
        ]
        [ HH.p_ [ HH.text $ "Placeholder Text" ]
        , HH.button
            [ HE.onClick \_ → do
                 case mStep of
                   Just OnStart → Hooks.modify_ stepId \_ → Just ToFinal
                   Just OnFinal → Hooks.modify_ stepId \_ → Just ToStart
                   _ → pure unit
            ]
            [ HH.text "Click Me!" ]
        ]


-- | Custom event for unimplemented animationend.
onAnimationEnd ∷ ∀ w a. (Event → a) → HP.IProp w a
onAnimationEnd = HE.handler (EventType "animationend")


-- | Utilty function for applying CSS classes.
css ∷ ∀ r a. String → HP.IProp ( class ∷ String | r ) a
css = HP.class_ <<< H.ClassName
