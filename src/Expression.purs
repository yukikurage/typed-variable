module Expression where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, iapply, imap, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Record (get, insert, set)
import Type.Proxy (Proxy(..))