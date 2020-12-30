module Halogen.Animated where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, EventType(..))


onAnimationEnd :: forall w a. (Event -> Maybe a) -> HP.IProp w a
onAnimationEnd = HE.handler (EventType "animationend")


type HTML w s m = H.ComponentHTML (Action w s m) s m

type DSL w s m = H.HalogenM (State w s m) (Action w s m) s (Message w) m

type Message w = w

type Slot w s = H.Slot Query (Message w) s


type Animations w s m =
  { start   :: String
  , toFinal :: String
  , final   :: String
  , toStart :: String
  , render  :: HTML w s m
  }


data AnimationState
  = Start
  | ToFinal
  | Final
  | ToStart


data Action w s m
  = ReceiveAnimations (Animations w s m)
  | HandleAnimationEnd
  | Raise w


type State w s m =
  { animations :: Animations w s m
  , current    :: AnimationState
  }


data Query q =
  ToggleAnimation q


initialState :: forall w s m. Animations w s m -> State w s m
initialState animations =
  { animations: animations
  , current: Start
  }


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


handleQuery :: forall w s m q. Query q -> DSL w s m (Maybe q)
handleQuery = case _ of
  (ToggleAnimation q) -> do
    state <- H.get
    case state.current of
      Start -> H.put $ state { current = ToFinal }
      Final -> H.put $ state { current = ToStart }
      _     -> pure unit
    pure $ Just q
