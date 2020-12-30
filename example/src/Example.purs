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


type Slots = (animated_box :: HN.Slot Action Int)


_animated_box = SProxy :: SProxy "animated_box"


data Action = Clicked Int


component :: forall query input output m. H.Component HH.HTML query input output m
component =
  H.mkComponent
  { initialState: const unit
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    }
  }


render :: forall m. Unit -> H.ComponentHTML Action Slots m
render _ =
  HH.div [ css "example" ]
  [ HH.slot _animated_box 0 HN.component
    { start: "opened"
    , toFinal: "move-and-lock"
    , final: "locked"
    , toStart: "move-and-open"
    , render: HH.div [ css "outer-box" , HE.onClick \_ -> Just $ HN.Raise $ Clicked 0 ] [ inner ]
    } Just
  ]
  where
    inner =
      HH.slot _animated_box 1 HN.component
        { start: "opened"
        , toFinal: "move-and-lock"
        , final: "locked"
        , toStart: "move-and-open"
        , render: HH.div [ css "inner-box" , HE.onClick \_ -> Just $ HN.Raise $ Clicked 1 ] [ ]
        } $ Just <<< HN.Raise


handleAction :: forall state output m. Action -> H.HalogenM state Action Slots output m Unit
handleAction = case _ of
  (Clicked i) -> void $ H.query _animated_box i $ H.tell HN.ToggleAnimation
