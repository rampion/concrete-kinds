{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Exp where
import Data.Dual

type family (^) = (e :: k -> k -> k) | e -> k where
  (^) = Dual (->)
  (^) = Exp_

data family Exp_ (a :: j -> k) (b :: j -> k) :: j -> k
newtype instance Exp_ a b x = Exp1 { getExp1 :: a x ^ b x }
newtype instance Exp_ a b x y = Exp2 { getExp2 :: a x y ^ b x y }
newtype instance Exp_ a b x y z = Exp3 { getExp3 :: a x y z ^ b x y z }
