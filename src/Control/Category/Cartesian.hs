{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
module Control.Category.Cartesian where

import qualified Control.Arrow as Shadowed ((&&&))
import qualified Prelude as Shadowed
import Prelude hiding (id, (.), fst, snd, Functor, fmap)

import Data.Void
import Control.Category

--import Control.Categorical.Functor
import Control.Categorical.Bifunctor
--import Data.Dual
--import Data.Universal


class Associative arrow bifunctor => Monoidal (arrow :: object -> object -> *) bifunctor where
  type Id arrow bifunctor :: object
  idl :: bifunctor (Id arrow bifunctor) a `arrow` a
  idr :: bifunctor a (Id arrow bifunctor) `arrow` a
  coidl :: a `arrow` bifunctor (Id arrow bifunctor) a 
  coidr :: a `arrow` bifunctor a (Id arrow bifunctor)

-- polykinded version of Control.Categorical.Cartesian.Cartesian from categories
class ( Symmetric arrow (Product arrow)
      , Monoidal arrow (Product arrow)
      ) => Cartesian (arrow :: object -> object -> *) where
  type Product arrow :: object -> object -> object
  fst :: Product arrow a b `arrow` a
  snd :: Product arrow a b `arrow` b

  diag :: a `arrow` Product arrow a a
  diag = id &&& id

  (&&&) :: c `arrow` a -> c `arrow` b -> c `arrow` Product arrow a b
  f &&& g = bimap f g . diag
  infixr 3 &&&

  {-# MINIMAL fst, snd, (diag | (&&&)) #-}

(***)
  :: Cartesian arrow
  => a `arrow` b
  -> c `arrow` d
  -> Product arrow a c `arrow` Product arrow b d
(***) = bimap

-- polykinded version of Control.Categorical.Cartesian.CoCartesian from categories
class ( Symmetric arrow (Sum arrow)
      , Monoidal arrow (Sum arrow)
      ) => CoCartesian (arrow :: object -> object -> *) where
  type Sum arrow :: object -> object -> object
  inl :: a `arrow` Sum arrow a b
  inr :: b `arrow` Sum arrow a b

  codiag :: Sum arrow a a `arrow` a
  codiag = id ||| id

  (|||) :: a `arrow` c -> b `arrow` c -> Sum arrow a b `arrow` c
  f ||| g = codiag . bimap f g
  infixr 2 |||

(+++)
  :: CoCartesian arrow
  => a `arrow` b
  -> c `arrow` d
  -> Sum arrow a c `arrow` Sum arrow b d
(+++) = bimap

{-
instance Monoidal arrow bifunctor => Monoidal (Dual arrow) bifunctor

instance Cartesian arrow => CoCartesian (Dual arrow) where
  type Sum (Dual arrow) = Product arrow
  inl = Dual1 fst
  inr = Dual1 snd
  codiag = Dual1 diag
  -}

{-
-- a category that has finite coproducts, weakened the same way as PreCartesian above was weakened
class (Monoidal k (Sum k), Symmetric k (Sum k)) => CoCartesian k where
    type Sum k :: * -> * -> *
    inl :: a `k` Sum k a b
    inr :: b `k` Sum k a b
    codiag :: Sum k a a `k` a
    (|||) :: k a c -> k b c -> Sum k a b `k` c

    codiag = id ||| id
    f ||| g = codiag . bimap f g
-}

instance Monoidal (->) (,) where
  type Id (->) (,) = ()
  idl = Shadowed.snd
  idr = Shadowed.fst
  coidl a = ((), a)
  coidr a = (a, ())

instance Cartesian (->) where
  type Product (->) = (,)
  fst = Shadowed.fst
  snd = Shadowed.snd
  (&&&) = (Shadowed.&&&)


instance Monoidal (->) Either where
  type Id (->) Either = Void
  idl = either absurd id
  idr = either id absurd
  coidl = Right
  coidr = Left

instance CoCartesian (->) where
  type Sum (->) = Either
  inl = Left
  inr = Right
  (|||) = either
