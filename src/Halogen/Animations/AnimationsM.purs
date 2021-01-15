module Halogen.Animations.AnimationsM where

import Prelude hiding (bind,discard,pure)

import Control.Applicative as Applicative
import Control.Monad.Free (Free, liftF)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Data.List (class Concat, type (:>), Nil', SymbolItem, kind List')


data AnimationsF n
  = CreateRef String (String -> n)


data AnimationsM (l :: List') a = AnimationsM (Free AnimationsF a)


derive instance functorAnimationsM :: Functor (AnimationsM l)


createRef :: forall l. IsSymbol l => SProxy l -> AnimationsM (SymbolItem l :> Nil') String
createRef s = AnimationsM $ liftF $ CreateRef (reflectSymbol s) \n -> n


bind :: forall a b x y z. Concat x y z => AnimationsM x a -> (a -> AnimationsM y b) -> AnimationsM z b
bind (AnimationsM ma) f = AnimationsM $ ma >>= \a -> case f a of AnimationsM mb -> mb


discard :: forall a x y z. Concat x y z  => AnimationsM x Unit -> (Unit -> AnimationsM y a) -> AnimationsM z a
discard = bind


pure :: forall a l. a -> AnimationsM l a
pure = AnimationsM <<< Applicative.pure


example :: forall l. AnimationsM (SymbolItem "hello" :> SymbolItem "world" :> l) Unit
example = do
  hello <- createRef (SProxy :: _ "hello")
  world <- createRef (SProxy :: _ "world")
  pure unit
