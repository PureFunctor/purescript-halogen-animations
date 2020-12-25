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


type HTML w m = H.ComponentHTML (Action w m) () m

type DSL w m = H.HalogenM (State w m) (Action w m) () (Message w) m

type Message w = w

type Slot w s = H.Slot Query (Message w) s


type Animations w m =
  { start   :: String
  , toFinal :: String
  , final   :: String
  , toStart :: String
  , render  :: HTML w m
  }


data AnimationState
  = Start
  | ToFinal
  | Final
  | ToStart


data Action w m
  = ReceiveAnimations (Animations w m)
  | HandleAnimationEnd
  | Raise w


type State w m =
  { animations :: Animations w m
  , current    :: AnimationState
  }


data Query q =
  ToggleAnimation q


initialState :: forall w m. Animations w m -> State w m
initialState animations =
  { animations: animations
  , current: Start
  }


render :: forall w m. State w m -> HTML w m
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


component :: forall w m. H.Component HH.HTML Query (Animations w m) (Message w) m
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    }
  }


handleAction :: forall w m. Action w m -> DSL w m Unit
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


handleQuery :: forall w m q. Query q -> DSL w m (Maybe q)
handleQuery = case _ of
  (ToggleAnimation q) -> do
    state <- H.get
    case state.current of
      Start -> H.put $ state { current = ToFinal }
      Final -> H.put $ state { current = ToStart }
      _     -> pure unit
    pure $ Just q
