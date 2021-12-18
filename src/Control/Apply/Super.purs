module Control.Apply.Super where

import Prelude

import Data.Functor.Super (class SuperFunctor, smap)

class (SuperFunctor x, SuperFunctor y, SuperFunctor z) <= SuperApply x y z where
  sapply :: forall a b. x (a -> b) -> y a -> z b

sapplyFirst :: forall x y z a b. SuperApply x y z => x a -> y b -> z a
sapplyFirst x y = const `smap` x `sapply` y

sapplySecnod :: forall x y z a b. SuperApply x y z => y a -> x b -> z b
sapplySecnod x y = const `smap` y `sapply` x