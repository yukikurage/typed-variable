module Data.Functor.Super
  ( (%$>)
  , (<$%)
  , class SuperFunctor
  , svoid
  , smap
  , svoidRight
  ) where

import Prelude

class SuperFunctor f where
  smap :: forall a b. (a -> b) -> f a -> f b

svoid :: forall f a. SuperFunctor f => f a -> f Unit
svoid = smap (const unit)

svoidRight :: forall f a b. SuperFunctor f => a -> f b -> f a
svoidRight a = smap (const a)

infixl 4 svoidRight as <$%

svoidLeft :: forall f b a. SuperFunctor f => f a -> b -> f b
svoidLeft = flip svoidRight

infixl 4 svoidRight as %$>