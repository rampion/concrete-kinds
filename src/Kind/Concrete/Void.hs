{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Kind.Concrete.Void where
import qualified Data.Void as Shadowed
import Data.ReprEq
import Data.Coerce

type family Void = (v :: k) | v -> k where
  Void = Shadowed.Void
  Void = Void_

class Void_ ~ Void => WrapVoid k where
  data Void_ :: (i -> k)

  newtypeVoid_ :: Void ~=~ (Void_ x :: k)
  default newtypeVoid_ :: Coercible Void (Void_ x :: k) => Void ~=~ (Void_ x :: k)
  newtypeVoid_ = IsCoercible

instance WrapVoid * where
  newtype Void_ x = Void1 { getVoid1 :: Void }

instance WrapVoid (j -> *) where
  newtype Void_ x y = Void2 { getVoid2 :: Void y }

instance WrapVoid (i -> j -> *) where
  newtype Void_ x y z = Void3 { getVoid3 :: Void y z }
