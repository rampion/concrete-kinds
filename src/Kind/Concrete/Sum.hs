{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Sum where

type family (+) = (c :: k -> k -> k) | c -> k where
  (+) = Either
  (+) = Sum_
infixl 6 +

data family Sum_ (a :: j -> k) (b :: j -> k) :: j -> k
newtype instance Sum_ a b x = Sum1 { getSum1 :: a x + b x }
newtype instance Sum_ a b x y = Sum2 { getSum2 :: a x y + b x y }
newtype instance Sum_ a b x y z = Sum3 { getSum3 :: a x y z + b x y z }
