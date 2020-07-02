{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
module Control.Categorical.Functor  where

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
      Functor (functor :: srcobject -> dstobject)
               (srcarrow :: srcobject -> srcobject -> *)
               (dstarrow :: dstobject -> dstobject -> *) where
  fmap :: (a `srcarrow` b) -> (functor a `dstarrow` functor b)

-- [0] Instances of Prelude's Functor class are endofunctors from the category
--     of types (->) to itself.
instance Shadowed.Functor functor => Functor functor (->) (->) where
  fmap = Shadowed.fmap

-- [1]  Instances of Category, cat, each define two functors from cat to the
--      category of types (->), (a `cat` _) and (_ `cat` b) (the hom functors).
--
--      However, we can't define an instance
--
--        instance Category cat => Functor (cat a) cat (->)
--
--      because it overlaps with the instance defined in [0] where
--      (functor ~ (->) a, cat ~ (->)).
--
--      (We only need to define one functor instance for each category because
--      we get an instance for its Dual automatically due to Dual cat's
--      instance of Category).
--
--      We could use the OVERLAPPABLE pragma to allow the two instances to coexist,
--      but it would interfere with GHC's ability to infer Functor instance
--      for nonspecified instances of Prelude's Functor, which would introduce
--      some impedence for users.
--
--      For example, this wouldn't compile:
--
--        >>> fmap' :: Shadowed.Functor functor => (a -> b) -> (functor a -> functor b)
--        >>> fmap' = fmap
--
--      We can get around this by defining a newtype wrapper for (->) and defining
--      functors from cat to this newtype's category.
--
--      This also lets us use DerivingVia to easily define functors from cat to the
--      category of types on a one by one basis. Not a perfect solution, but a
--      workable compromise.
newtype Hask a b = Hask { getHask :: a -> b }
  deriving Category

instance Category cat => Functor (cat x) cat Hask where
  fmap cab = Hask $ \cxa -> cab . cxa

deriving via Hask instance Monad m => Functor (Kleisli m x) (Kleisli m) (->)

-- Contravariant functors are just functors that flip one of the arrows around
type Contravariant functor srcarrow dstarrow =
  (Flippable srcarrow, Functor functor (Flipped srcarrow) dstarrow)

contramap :: Contravariant functor srcarrow dstarrow
          => (b `srcarrow` a) -> (functor a `dstarrow` functor b)
contramap = coerceWith (domain (sym newtypeFlipped)) fmap

-- [2] All 'core' contravariant functors are Functors
instance Shadowed.Contravariant functor => Functor functor (Dual (->)) (->) where
  fmap = coerceWith (domain newtypeDual) contramap

class (Functor functor prrow (Universal arrow)) => Peafunctor functor prrow arrow where
  pmap :: (a `prrow` b) -> (functor a x `arrow` functor b x)
  pmap = getUniversal . fmap

instance (Functor functor prrow (Universal arrow)) => Peafunctor functor prrow arrow

contrapmap :: (Peafunctor functor (Flipped prrow) arrow, Flippable prrow)
           => (a `prrow` b) -> functor b x `arrow` functor a x
contrapmap = coerceWith (domain (sym newtypeFlipped)) pmap


class (forall x. Functor (functor x) qrrow arrow) => Kewfunctor functor qrrow arrow where
  qmap :: c `qrrow` d -> functor x c `arrow` functor x d
  qmap = fmap

instance (forall x. Functor (functor x) qrrow arrow) => Kewfunctor functor qrrow arrow

contraqmap :: (Kewfunctor functor (Flipped qrrow) arrow, Flippable qrrow)
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

-- [3] Representational functors are Functors
instance
    ( forall a b. Coercible a b => Coercible (f a) (f b)
    ) => Functor f Coercion Coercion where
  fmap Coercion = Coercion

parameter :: a `Coercion` b -> a x `Coercion` b x
parameter Coercion = Coercion

-- [4] Universality; Functors can drop extraneous parameters
instance
    -- rotcnuf is needed to prove to GHC that Flipped only depends on its first parameter
    ( rotcnuf ~ Flipped functor
    , forall q. Functor (rotcnuf q) prrow arrow
    , Flippable functor
    , Birepresentational arrow
    ) => Functor functor prrow (Universal arrow) where

  fmap f = Universal $ coerceWith proof fmap f where
    proof :: Coercion
      (prrow b a -> arrow (rotcnuf q b) (rotcnuf q a))
      (prrow b a -> arrow (functor b q) (functor a q))
    proof = codomain (domain (sym newtypeFlipped) . codomain newtypeFlipped)

-- [5] Duals; we can flip both arrows
instance ( Functor functor (Flipped srcarrow) dstarrow
         , Category srcarrow
         , Flippable srcarrow
         ) => Functor functor srcarrow (Dual dstarrow) where
  fmap = coerceWith proof fmap where
    proof :: Coercion
       (Flipped srcarrow b a -> dstarrow (functor b) (functor a))
       (srcarrow a b -> Dual dstarrow (functor a) (functor b))
    proof = domain (sym newtypeFlipped) . codomain (sym newtypeDual)
