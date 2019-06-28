{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Kind.Concrete.Unit where
import Data.Type.Coercion
import Data.Coerce

type family Unit = (u :: k) | u -> k where
  Unit = ()
  Unit = Unit_

class Unit_ ~ Unit => WrapUnit k where
  data Unit_ :: (j -> k)

  newtypeUnit_ :: Unit ~=~ (Unit_ x :: k)
  default newtypeUnit_ :: Coercible Unit (Unit_ x :: k) => Unit ~=~ (Unit_ x :: k)
  newtypeUnit_ = Coercion

instance WrapUnit * where
  newtype Unit_ x = Unit1 { getUnit1 :: Unit }

instance WrapUnit (j -> *) where
  newtype Unit_ x y = Unit2 { getUnit2 :: Unit y }

instance WrapUnit (i -> j -> *) where
  newtype Unit_ x y z = Unit3 { getUnit3 :: Unit y z }
