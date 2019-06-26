{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Const where
import Kind.Concrete.Natural
import Control.Category.Coercible
import qualified Data.Functor.Const as Shadowed

type family Const = (c :: k -> j -> k) | c -> k where
  Const = Shadowed.Const
  Const = Const_

data family Const_ (a :: j -> k) (b :: i) :: j -> k
newtype instance Const_ a b x = Const1 { getConst1 :: a x }
newtype instance Const_ a b x y = Const2 { getConst2 :: a x y }
newtype instance Const_ a b x y z = Const3 { getConst3 :: a x y z }

_Const :: a <~> Const a b
_Const = coerce
