{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Kind.Concrete.Const where
import qualified Data.Functor.Const as Shadowed
import Data.Type.Coercion
import Data.Coerce

type family Const = (c :: k -> j -> k) | c -> k where
  Const = Shadowed.Const
  Const = Const_

class WrapConst k where
  data Const_ (a :: j -> k) (b :: i) :: j -> k

  newtypeConst_ :: forall (a :: k) b. a ~=~ Const a b
  default newtypeConst_ :: forall (a :: k) b. Coercible a (Const a b) => a ~=~ Const a b
  newtypeConst_ = Coercion

instance WrapConst * where
  newtype Const_ a b x = Const1 { getConst1 :: a x }
instance WrapConst (k -> *) where
  newtype Const_ a b x y = Const2 { getConst2 :: a x y }
instance WrapConst (j -> k -> *) where
  newtype Const_ a b x y z = Const3 { getConst3 :: a x y z }
