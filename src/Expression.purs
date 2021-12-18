module Expression
  ( (:=)
  , (@=)
  , Expression
  , define
  , iliftA2
  , ipure2
  , ipure3
  , read
  , run
  , runExpression
  , write
  ) where

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

data Expression p q a = Expression
  (Record p -> { variables :: Record q, result :: a })

type YFunction p q r s x y = Expression p q (x -> Expression r s y)

yapply
  :: Expression p q (x -> Expression r s y)
  -> Expression p t x
  -> Expression p s y

--------------
-- External --
--------------

iliftA2
  :: forall f x y z p q r
   . IxApplicative f
  => (x -> y -> z)
  -> f p q x
  -> f q r y
  -> f p r z
iliftA2 f x y = f `imap` x `iapply` y

ipure2 :: forall p x y f. IxApplicative f => (x -> y) -> x -> f p p y
ipure2 f = ipure <<< f

ipure3
  :: forall p q x y z f
   . IxApplicative f
  => (x -> y -> z)
  -> x
  -> f p p (y -> f q q z)
ipure3 f = ipure <<< ipure2 <<< f

instance IxFunctor Expression where
  imap f (Expression g) = Expression \r ->
    let
      { variables, result } = g r
    in
      { variables, result: f result }

instance IxApply Expression where
  iapply (Expression f) (Expression g) = Expression \r ->
    let
      { variables: v0, result: r0 } = f r
      { variables: v1, result: r1 } = g v0
    in
      { variables: v1, result: r0 r1 }

instance IxApplicative Expression where
  ipure result = Expression \variables -> { variables, result }

instance IxBind Expression where
  ibind (Expression f) g = Expression \r ->
    let
      { variables: v0, result: r0 } = f r
      Expression h = g r0
    in
      h v0

instance IxMonad Expression

runExpression :: forall p x. Expression () p x -> x
runExpression (Expression f) = (f {}).result

--------------
-- Internal --
--------------

-- | 変数を定義する
define
  :: forall s p q r x
   . IsSymbol s
  => Lacks s q
  => Cons s x q r
  => Proxy s
  -> Expression p q x
  -> Expression p r Unit
define s (Expression f) = Expression \r ->
  let
    { variables, result } = f r
  in
    { variables: insert s result variables, result: unit }

infix 2 define as :=

read
  :: forall p q x s. Cons s x p q => IsSymbol s => Proxy s -> Expression q q x
read proxy = Expression \variables -> { variables, result: get proxy variables }

write
  :: forall p q r x s
   . Cons s x r q
  => IsSymbol s
  => Proxy s
  -> Expression p q x
  -> Expression p q Unit
write proxy (Expression f) = Expression \r ->
  let
    { variables, result } = f r
  in
    { variables: set proxy result variables, result: unit }

infix 2 write as @=

run
  :: forall p q r x
   . Expression p q (Expression () r x)
  -> Expression p q x
run (Expression f) = Expression \r ->
  let
    { variables, result: Expression g } = f r
    { result } = g {}
  in
    { variables, result }
