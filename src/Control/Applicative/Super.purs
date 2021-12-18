module Control.Applicative.Super where

import Prelude

import Data.Functor.Super (class SuperFunctor)

class SuperFunctor x <= SuperApplicative x where
  spure :: forall a. a -> x a

swhen :: forall x. SuperApplicative x => Boolean -> x Unit -> x Unit
swhen frag x = if frag then x else spure unit

sunless :: forall x. SuperApplicative x => Boolean -> x Unit -> x Unit
sunless = not >>> swhen