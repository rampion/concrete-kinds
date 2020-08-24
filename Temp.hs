{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- prevent GHC from complaining about Birepresentational
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints -fprint-potential-instances #-}
module Control.Categorical.Functor 
  ( Functor(..)
  , Contravariant, contramap
  , Kewfunctor, pmap, pmap', qmap, contrapmap, contraqmap
  , Representational, Birepresentational, domain, codomain, parameter
  , CategoryInstance(..)
  ) where

import qualified Prelude as Shadowed
import qualified Data.Functor.Contravariant as Shadowed

import Prelude hiding (id, (.), Functor(..))
import Control.Arrow (Kleisli())
import Control.Category
import Data.Coerce
import Data.Type.Coercion

import Data.Universal
import Data.Dual

class (Category srcarrow, Category dstarrow) => 
      Functor (srcarrow :: srcobject -> srcobject -> *)
              (dstarrow :: dstobject -> dstobject -> *) 
              (functor :: srcobject -> dstobject) where
  fmap :: (a `srcarrow` b) -> (functor a `dstarrow` functor b)

-- [1] All 'core' functors are Functors in (->), (->)
instance Shadowed.Functor functor => Functor (->) (->) functor where
  fmap = Shadowed.fmap

-- Contravariant functors are just functors that flip one of the arrows around
type Contravariant srcarrow dstarrow functor =
  (Flippable srcarrow, Functor (Flipped srcarrow) dstarrow functor)

contramap :: Contravariant srcarrow dstarrow functor
          => (b `srcarrow` a) -> (functor a `dstarrow` functor b)
contramap = coerceWith (domain (sym newtypeFlipped)) fmap

-- [2] All 'core' contravariant functors are Functors
instance Shadowed.Contravariant functor => Functor (Dual (->)) (->) functor where
  fmap = coerceWith (domain newtypeDual) contramap

class (Flippable functor, forall x. Functor prrow arrow (functor `Flipped` x))
      => Peafunctor prrow arrow functor where
  pmap :: (a `prrow` b) -> (functor a x `arrow` functor b x)
  pmap = _
  -- pmap f = getUniversal (fmap f)
  {-
  default pmap :: Birepresentational arrow
               => (a `prrow` b) -> (functor a x `arrow` functor b x)
  pmap = coerceWith (codomain (domain (sym newtypeFlipped) . codomain newtypeFlipped)) fmap
  -}

pmap' :: Functor prrow (Universal arrow) functor
     => (a `prrow` b) -> (functor a x `arrow` functor b x)
pmap' = getUniversal . fmap

contrapmap :: Peafunctor (Flipped prrow) arrow functor
           => (a `prrow` b) -> functor b x `arrow` functor a x
contrapmap = coerceWith (domain (sym newtypeFlipped)) pmap

class (forall x. Functor qrrow arrow (functor x)) => Kewfunctor qrrow arrow functor where
  qmap :: c `qrrow` d -> functor x c `arrow` functor x d
  qmap = fmap
  -- qmap = coerceWith (codomain (domain (sym newtypeFlipped) . codomain newtypeFlipped)) pmap where

contraqmap :: ( Flippable functor
              , Contravariant qrrow (Universal arrow) (Flipped functor)
              , Birepresentational arrow
              )
           => c `qrrow` d -> functor x d `arrow` functor x c
contraqmap = coerceWith (domain (sym newtypeFlipped)) qmap


-- Representational functors are type constructors that have a parameter with
-- a representational role

class    (forall a b. Coercible a b => Coercible (op a) (op b)) => Representational op
instance (forall a b. Coercible a b => Coercible (op a) (op b)) => Representational op

class Representational op => Pearepresentational op where
  domain :: Coercion a b -> Coercion (b `op` c) (a `op` c)
  domain = parameter . sym . fmap

instance Representational op => Pearepresentational op

class (forall x. Representational (op x)) => Kewrepresentational op where
  codomain :: Coercion a b -> Coercion (c `op` a) (c `op` b)
  codomain = fmap

instance (forall x. Representational (op x)) => Kewrepresentational op


-- Birepresentational functors are type constructors that have two parameters
-- with representational roles
type Birepresentational op = (Pearepresentational op, Kewrepresentational op)

{-
type Representational = Functor Coercion Coercion
type Birepresentational f = 
  ( Functor Coercion Coercion f
  , Functor Coercion Coercion (f x)
  , Flippable f
  )
-}

-- [3] Representational functors are Functors
instance
    ( forall a b. Coercible a b => Coercible (f a) (f b)
    ) => Functor Coercion Coercion f where
  fmap Coercion = Coercion

parameter :: a `Coercion` b -> a x `Coercion` b x
parameter Coercion = Coercion

-- [4] Universality; Functors can drop extraneous parameters
instance
    -- functor' is needed to prove to GHC that Flipped only depends on its first parameter
    ( functor' ~ Flipped functor
    , forall q. Functor prrow arrow (functor' q)
    , Flippable functor
    , Birepresentational arrow
    ) => Functor prrow (Universal arrow) functor where

  fmap f = Universal $ coerceWith proof fmap f where
    proof :: Coercion
      (prrow b a -> arrow (functor' q b) (functor' q a))
      (prrow b a -> arrow (functor b q) (functor a q))
    proof = codomain (domain (sym newtypeFlipped) . codomain newtypeFlipped)

-- [5] Duals; we can flip both arrows
instance ( Functor (Flipped srcarrow) dstarrow functor
         , Category srcarrow
         , Flippable srcarrow
         ) => Functor srcarrow (Dual dstarrow) functor where
  fmap = coerceWith proof fmap where
    proof :: Coercion
       (Flipped srcarrow b a -> dstarrow (functor b) (functor a))
       (srcarrow a b -> Dual dstarrow (functor a) (functor b))
    proof = domain (sym newtypeFlipped) . codomain (sym newtypeDual)

--------------------------------------------------------------------------------

newtype CategoryInstance (cat :: k -> k -> *) a b = CategoryInstance { getCategoryInstance :: cat a b }
  deriving Category

instance Category cat => Functor cat (->) (CategoryInstance cat x) where
  fmap f g = CategoryInstance f . g

instance Category cat => Functor (Dual cat) (->) (CategoryInstance cat `Dual` x) where
  fmap (Dual1 f) (Dual1 g) = Dual1 (g . CategoryInstance f)

deriving via (CategoryInstance (Kleisli m) x)
  instance Monad m => Functor (Kleisli m) (->) (Kleisli m x)

deriving via (CategoryInstance (Kleisli m) `Dual` x)
  instance Monad m => Functor (Dual (Kleisli m)) (->) (Kleisli m `Dual` x)

--------------------------------------------------------------------------------

{-
unwrapDualToDual 
  :: forall (functor :: pbject -> qbject -> object)
            (arrow :: object -> object -> *)
            (a :: pbject)
            (b :: pbject)
            (c :: qbject)
            (d :: qbject).
  ( HasDual object, Birepresentational arrow )
  => (Dual functor c a `arrow` Dual functor d b) `Coercion`
         (functor a c `arrow` functor b d)
unwrapDualToDual 
  = (newtypeDual                   :: Dual arrow (functor b d) (functor a c) `Coercion` arrow (functor a c) (functor b d))
  . (pmap newtypeDual              :: Dual arrow (Dual functor d b) (functor a c) `Coercion` Dual arrow (functor b d) (functor a c))
  . (sym newtypeDual               :: arrow (functor a c) (Dual functor d b) `Coercion` Dual arrow (Dual functor d b) (functor a c))
  . (parameter (fmap newtypeDual)  :: arrow (Dual functor c a) (Dual functor d b) `Coercion` arrow (functor a c) (Dual functor d b))
-}
