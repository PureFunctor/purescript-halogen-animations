module Example where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Animated as HN
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


css :: forall a r. String -> HP.IProp (class :: String | r) a
css = HP.class_ <<< H.ClassName


type Slots = (animated_box :: HN.Slot Void Unit)


_animated_box = SProxy :: SProxy "animated_box"


data Action = Clicked


component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }


render :: forall state m. state -> HH.ComponentHTML Action Slots m
render _ =
  HH.div [ css "example" , HE.onClick \_ -> Just Clicked ]
  [ HH.slot _animated_box unit HN.component
    { start: "opened"
    , toFinal: "move-and-lock"
    , final: "locked"
    , toStart: "move-and-open"
    , render: HH.div [ css "box" ] [ ]
    } absurd
  ]


handleAction :: forall state output m. Action -> H.HalogenM state Action Slots output m Unit
handleAction = case _ of
  Clicked -> do
    _ <- H.query _animated_box unit $ H.tell HN.ToggleAnimation
    pure unit
