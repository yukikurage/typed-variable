module Main where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect (Effect)
import Effect.Class.Console (logShow)
import Expression (iliftA2, read, runExpression, (:=), (@=))
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = logShow $ runExpression testExpr

_x = Proxy :: Proxy "x"
_y = Proxy :: Proxy "y"

_1 = ipure 1

_2 = ipure 2

_3 = ipure 3

_add = iliftA2 add

infixl 6 _add as <+>

testExpr = Ix.do
  _x := Ix.do
    _y := _1
    read _y
  read _y