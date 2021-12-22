module Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Type.Proxy (Proxy(..))
import Y (Y, read, run, runY, ($$), (:=), (@=), (@@), (\->))

main :: Effect Unit
main = logShow $ runY {} test

_x = pure Proxy :: forall m. Applicative m => m (Proxy "x")
_y = pure Proxy :: forall m. Applicative m => m (Proxy "y")
_z = pure Proxy :: forall m. Applicative m => m (Proxy "z")
_f = pure Proxy :: forall m. Applicative m => m (Proxy "f")
_g = pure Proxy :: forall m. Applicative m => m (Proxy "g")
_h = pure Proxy :: forall m. Applicative m => m (Proxy "h")

_a = pure Proxy :: forall m. Applicative m => m (Proxy "a")
_b = pure Proxy :: forall m. Applicative m => m (Proxy "b")
_c = pure Proxy :: forall m. Applicative m => m (Proxy "c")

_1 :: forall t63. Applicative t63 => t63 Int
_1 = pure 1

_2 :: forall t60. Applicative t60 => t60 Int
_2 = pure 2

_3 :: forall t57. Applicative t57 => t57 Int
_3 = pure 3

_add = pure add

test :: Y () Int
test =
  _a := pure (_x := _2)
    $ _b := pure (_g := _y \-> _add @@ read _x @@ read _y)
    $ _c := pure (read _g @@ read _x)
    $ run @@ (read _a @@ (read _b @@ read _c))
