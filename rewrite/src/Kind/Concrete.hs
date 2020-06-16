-- {-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DerivingVia #-}
module Kind.Concrete where

import Prelude hiding ((.), id)
import Control.Category
import qualified Data.Void as Shadowed

type family (~~>) = (arrow :: object -> object -> *) | arrow -> object where
  (~~>) = (->)
  (~~>) = Arrow
infixr 0 ~~>

type family Unit = (unit :: object) | unit -> object where
  Unit = ()
  Unit = Nonary ()

type family Void = (void :: object) | void -> object where
  Void = Shadowed.Void
  Void = Nonary Shadowed.Void

data family Arrow :: object -> object -> *
data family Nonary (a :: *) :: j -> k

-- polykinded version of Control.Categorical.Object.HasTerminalObject from categories
class Category arrow => HasTerminalObject (arrow :: object -> object -> *) where
  type Terminal arrow :: object
  terminate :: a `arrow` Terminal arrow

-- polykinded version of Control.Categorical.Object.HasInitialObject from categories
class Category arrow => HasInitialObject (arrow :: object -> object -> *) where
  type Initial arrow :: object
  initiate :: Initial arrow `arrow` a

{- object ~ * -----------------------------------------------------------------}

instance HasTerminalObject (->) where
  type Terminal (->) = Unit
  terminate = const ()

instance HasInitialObject (->) where
  type Initial (->) = Void
  initiate = Shadowed.absurd

{- object ~ k -> * ------------------------------------------------------------}
  
newtype instance Arrow a b = Arrow1 { getArrow1 :: forall x. a x -> b x }
newtype instance Nonary a x = Nonary1 { getNonary1 :: a }

instance Category (Arrow :: (k -> *) -> (k -> *) -> *) where
  Arrow1 f . Arrow1 g = Arrow1 (f . g)
  id = Arrow1 id

instance HasTerminalObject (Arrow :: (k -> *) -> (k -> *) -> *) where
  type Terminal Arrow = Unit
  terminate = Arrow1 \_ -> Nonary1 ()

instance HasInitialObject (Arrow :: (k -> *) -> (k -> *) -> *) where
  type Initial Arrow = Void
  initiate = Arrow1 \case

{- object ~ j -> k -> * -------------------------------------------------------}

newtype instance Arrow a b = Arrow2 { getArrow2 :: forall x y. a x y -> b x y }
newtype instance Nonary a x y = Nonary2 { getNonary2 :: a }

instance Category (Arrow :: (j -> k -> *) -> (j -> k -> *) -> *) where
  Arrow2 f . Arrow2 g = Arrow2 (f . g)
  id = Arrow2 id

instance HasTerminalObject (Arrow :: (j -> k -> *) -> (j -> k -> *) -> *) where
  type Terminal Arrow = Unit
  terminate = Arrow2 \_ -> Nonary2 ()

instance HasInitialObject (Arrow :: (j -> k -> *) -> (j -> k -> *) -> *) where
  type Initial Arrow = Void
  initiate = Arrow2 \case

{- object ~ i -> j -> k -> * --------------------------------------------------}

newtype instance Arrow a b = Arrow3 { getArrow3 :: forall x y z. a x y z -> b x y z }
newtype instance Nonary a x y z = Nonary3 { getNonary3 :: a }

instance Category (Arrow :: (i -> j -> k -> *) -> (i -> j -> k -> *) -> *) where
  Arrow3 f . Arrow3 g = Arrow3 (f . g)
  id = Arrow3 id

instance HasTerminalObject (Arrow :: (i -> j -> k -> *) -> (i -> j -> k -> *) -> *) where
  type Terminal Arrow = Unit
  terminate = Arrow3 \_ -> Nonary3 ()

instance HasInitialObject (Arrow :: (i -> j -> k -> *) -> (i -> j -> k -> *) -> *) where
  type Initial Arrow = Void
  initiate = Arrow3 \case

{------------------------------------------------------------------------------}

{-
type family (-->) = (exponential :: object -> object -> object) | exponential -> object where
  (-->) = (->)
  (-->) = Exponential
infixr 1 -->
data family Exponential :: object -> object -> object

class Concrete object where

  wrapArrow :: forall (a :: k -> object) (b :: k -> object). (forall x. a x ~~> b x) -> (a ~~> b)
  unwrapArrow  :: forall (a :: k -> object) (b :: k -> object). (a ~~> b) -> (forall x. a x ~~> b x)

pattern Universal :: forall (a :: k -> object) (b :: k -> object).
                     Concrete object => (forall x. a x ~~> b x) -> (a ~~> b)
pattern Universal arrow <- (unwrapArrow -> arrow)
  where Universal arrow = wrapArrow arrow

instance Concrete * where
  wrapArrow = Arrow1
  unwrapArrow = getArrow1

instance Concrete (k -> *) where
  wrapArrow arrow  = Arrow2 (getArrow1 arrow)
  unwrapArrow arrow   = Arrow1 (getArrow2 arrow)

instance Concrete (j -> k -> *) where
  wrapArrow arrow = Arrow3 (getArrow2 arrow)
  unwrapArrow arrow  = Arrow2 (getArrow3 arrow)
-}
