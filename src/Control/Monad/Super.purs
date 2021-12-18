module Control.Monad.Super where

import Prelude

import Control.Applicative.Super (class SuperApplicative, spure)
import Control.Bind.Super (class SuperBind, sbind, (%>>=))

class
  ( SuperApplicative x
  , SuperApplicative y
  , SuperApplicative z
  , SuperBind x y z
  ) <=
  SuperMonad x y z
