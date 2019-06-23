{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Product where

type family (×) = (p :: k -> k -> k) | p -> k where
  (×) = (,)
  (×) = Product_
infixl 7 ×

data family Product_ (a :: j -> k) (b :: j -> k) :: j -> k
newtype instance Product_ a b x = Product1 { getProduct1 :: a x × b x }
newtype instance Product_ a b x y = Product2 { getProduct2 :: a x y × b x y }
newtype instance Product_ a b x y z = Product3 { getProduct3 :: a x y z × b x y z }
