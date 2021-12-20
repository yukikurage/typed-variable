module Main where

import Prelude

import Control.Applicative.Indexed (iapply, imap, ipure)
import Control.Bind.Indexed ((=<<:))
import Control.Monad.Indexed.Qualified as Ix
import Data.Array (range)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Expression (Expression(..), iliftA2, introduce, ipure3, read, run, runExpression, (++>), (:=), (@=))
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = logShow $ runExpression testExpr

_x = Proxy :: Proxy "x"
_y = Proxy :: Proxy "y"
_z = Proxy :: Proxy "z"
_f = Proxy :: Proxy "f"
_g = Proxy :: Proxy "g"
_h = Proxy :: Proxy "h"

_a = Proxy :: Proxy "a"

_1 = ipure 1

_2 = ipure 2

_3 = ipure 3

_add = ipure $ add `imap` read _x `iapply` read _y

testExpr :: Expression () _ Int
testExpr = Ix.do
  _x := _2
  _a := introduce _x ++> ipure (_y := _1) ++> _add
  run (read _a)
