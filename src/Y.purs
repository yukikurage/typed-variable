module Y
  ( ($$)
  , (:=)
  , (@=)
  , (@@)
  , (\->)
  , Y
  , define
  , lambda
  , read
  , run
  , runY
  , substitute
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Lacks)
import Record (get, insert, set)
import Type.Proxy (Proxy)

data Y p x = Y (Record p -> x)

runY :: forall x p. Record p -> Y p x -> x
runY r (Y f) = f r

instance Functor (Y p) where
  map f (Y g) = Y \p -> f $ g p

instance Apply (Y p) where
  apply (Y f) (Y g) = Y \p ->
    let
      h = f p
    in
      h $ g p

instance Applicative (Y p) where
  pure x = Y $ const x

lambda
  :: forall a x y p q
   . IsSymbol a
  => Cons a x p q
  => Lacks a p
  => Y p (Proxy a)
  -> Y q y
  -> Y p (x -> y)
lambda (Y f) (Y g) = Y \p -> \x ->
  let
    a = f p
  in
    g $ insert a x p

infixr 3 lambda as \->

infixl 9 apply as @@
infixr 1 apply as $$

instance Bind (Y p) where
  bind (Y f) g = Y \p ->
    let
      x = f p
      Y h = g x
    in
      h p

instance Monad (Y p)

define
  :: forall a p q x y
   . Cons a x q p
  => Lacks a q
  => IsSymbol a
  => Y q (Proxy a)
  -> Y q x
  -> Y p y
  -> Y q y
define (Y f) (Y g) (Y h) = Y \q ->
  let
    a = f q
    x = g q
  in
    h $ insert a x q

infix 2 define as :=

substitute
  :: forall a p q x y
   . Cons a x q p
  => IsSymbol a
  => Y p (Proxy a)
  -> Y p x
  -> Y p y
  -> Y p y
substitute (Y f) (Y g) (Y h) = Y \p ->
  let
    a = f p
    x = g p
  in
    h $ set a x p

infix 2 substitute as @=

read :: forall p q a x. IsSymbol a => Cons a x q p => Y p (Proxy a) -> Y p x
read (Y f) = Y \p ->
  let
    a = f p
  in
    get a p

run :: forall p x. Y p (Y () x -> x)
run = Y \_ -> \(Y f) -> f {}