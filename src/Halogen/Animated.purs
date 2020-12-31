module Halogen.Animated
  ( onAnimationEnd
  , HTML
  , DSL
  , Message
  , Slot
  , Animations
  , AnimationState(..)
  , Action(..)
  , State
  , Query(..)
  , initialState
  , render
  , component
  , handleAction
  , handleQuery
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, EventType(..))


-- | Halogen event handler for `animationend`.
-- |
-- | - `w` extends the `IProp`.
-- | - `a` is the `action` that the `IProp` invokes.
onAnimationEnd :: forall w a. (Event -> Maybe a) -> HP.IProp w a
onAnimationEnd = HE.handler (EventType "animationend")


-- | ComponentHTML type to be passed to `Animations`.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
type HTML w s m = H.ComponentHTML (Action w s m) s m


-- | HalogenM type used internally by the component.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
type DSL w s m = H.HalogenM (State w s m) (Action w s m) s (Message w) m


-- | Messages raised by the component from the internal `HTML` type.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
type Message w = w


-- | Helper type for defining slots in client code.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
type Slot w s = H.Slot Query (Message w) s


-- | A record of animations to be toggled from and to.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
type Animations w s m =
  { start   :: String
  , toFinal :: String
  , final   :: String
  , toStart :: String
  , render  :: HTML w s m
  }


-- | The current state of the animation.
data AnimationState
  = Start
  | ToFinal
  | Final
  | ToStart


-- | Internal actions to be performed by the component.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
data Action w s m
  = ReceiveAnimations (Animations w s m)
  | HandleAnimationEnd
  | Raise w


-- | Internal state of the component.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
type State w s m =
  { animations :: Animations w s m
  , current    :: AnimationState
  }


-- | Query type used to interact with the component.
data Query q =
  ToggleAnimation q


-- | Internal initial state of the component.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
initialState :: forall w s m. Animations w s m -> State w s m
initialState animations =
  { animations: animations
  , current: Start
  }


-- | Internal render function of the component.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
render :: forall w s m. State w s m -> HTML w s m
render { animations , current } =
  HH.div
  [ onAnimationEnd \_ -> Just HandleAnimationEnd
  , HP.class_ <<< H.ClassName $ animationClass
  ]
  [ animations.render ]
  where
    animationClass = case current of
      Start -> animations.start
      ToFinal -> animations.toFinal
      Final -> animations.final
      ToStart -> animations.toStart


-- | Component for adding animations.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
component :: forall w s m. H.Component HH.HTML Query (Animations w s m) (Message w) m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    }
  }

-- | Internal `action` handler for toggling animations
-- | and raising messages.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
handleAction :: forall w s m. Action w s m -> DSL w s m Unit
handleAction = case _ of
  ReceiveAnimations animations -> do
    state <- H.get
    H.put $ state { animations = animations }
  HandleAnimationEnd -> do
    state <- H.get
    case state.current of
      ToFinal -> H.put $ state { current = Final }
      ToStart -> H.put $ state { current = Start }
      _       -> pure unit
  Raise message -> H.raise message


-- | Internal `query` handler for toggling animations.
-- |
-- | - `w` is the `message` to be raised by the wrapped
-- |   component that's actually an `action`.
-- | - `s` is the `state` of the component.
-- | - `m` is the `monad` of the component.
handleQuery :: forall w s m q. Query q -> DSL w s m (Maybe q)
handleQuery = case _ of
  (ToggleAnimation q) -> do
    state <- H.get
    case state.current of
      Start -> H.put $ state { current = ToFinal }
      Final -> H.put $ state { current = ToStart }
      _     -> pure unit
    pure $ Just q
