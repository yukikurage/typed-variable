module Control.Bind.Super where

import Prelude

import Control.Apply.Super (class SuperApply)

class SuperApply x y z <= SuperBind x y z where
  sbind :: forall a b. x a -> (a -> y b) -> z b

infixl 1 sbind as %>>=

sbindFlipped :: forall x y z a b. SuperBind x y z => (a -> y b) -> x a -> z b
sbindFlipped = flip sbind

infixr 1 sbindFlipped as =<<%

composesKleisli
  :: forall a b c x y z. SuperBind x y z => (a -> x b) -> (b -> y c) -> a -> z c
composesKleisli f g a = f a %>>= g

infixl 1 composesKleisli as %>=>

composesKleisliFlipped
  :: forall a b c x y z. SuperBind x y z => (b -> y c) -> (a -> x b) -> a -> z c
composesKleisliFlipped = flip composesKleisli

infixl 1 composesKleisliFlipped as <=<%