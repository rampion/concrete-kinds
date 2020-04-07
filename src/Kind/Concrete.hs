{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete where

import Data.Coerce
import GHC.Types (Constraint)

import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Distributive
import Control.Category.Exponential
import Control.Category.Final
import Control.Category.Initial
import Control.Category.Strong
import Data.Iso
import Kind.Concrete.Natural
import Kind.Concrete.Product
import Kind.Concrete.Sum
import Kind.Concrete.Unit
import Kind.Concrete.Void

type (<~>) = Iso (~>)
infix 0 <~>

class ( Distributive ((~>) :: k -> k -> *) 
      , Cartesian ((~>) :: k -> k -> *) 
      , Cocartesian ((~>) :: k -> k -> *) 
      , Initial ((~>) :: k -> k -> *) 
      , Final ((~>) :: k -> k -> *) 
      , Exponential ((~>) :: k -> k -> *) 
      , Product ((~>) :: k -> k -> *) ~ (×)
      , Coproduct ((~>) :: k -> k -> *) ~ (+)
      , Terminal ((~>) :: k -> k -> *) ~ Unit
      , Coterminal ((~>) :: k -> k -> *) ~ Void
      , Exp ((~>) :: k -> k -> *) ~ (^)
      ) => Concrete k where

  _Coerce :: forall (a :: k) b. Coercible a b => a <~> b

  -- FIXME: can't get the kind of Data.Functor.Compose to line up
  data (∘) :: forall i j. (j -> k) -> (i -> j) -> i -> k
  infixr 9 ∘

  {-
  data (^) :: k -> k -> k
  infixr 8 ^

  _Curry :: forall (a :: k) b c. ((a × b) ~> c) <-> (a ~> c ^ b)
  -}


  data Free :: (k -> Constraint) -> k -> k
  call :: forall (a :: k) (c :: k -> Constraint). a ~> Free c a
  interpret :: forall (a :: k) b (c :: k -> Constraint). c b => (a ~> b) -> Free c a ~> b

  data Fix :: (k -> k) -> k

