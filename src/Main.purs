module Main where

import Prelude

import Control.Applicative.Indexed (iapply, ipure)
import Control.Bind.Indexed ((=<<:))
import Control.Monad.Indexed.Qualified as Ix
import Effect (Effect)
import Effect.Class.Console (logShow)
import Expression
  ( Expression
  , iliftA2
  , ipure3
  , read
  , run
  , runExpression
  , (:=)
  , (@=)
  )
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = logShow $ runExpression testExpr

_x = Proxy :: Proxy "x"
_y = Proxy :: Proxy "y"
_z = Proxy :: Proxy "z"

_1 = ipure 1

_2 = ipure 2

_3 = ipure 3

_add = ipure3 add

testExpr = Ix.do
  _x := ipure Ix.do
    _z := _2
    _z @= ((_add =<<: _1) `iapply` (read _z))
    read _z
  run (read _x)