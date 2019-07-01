{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
module Kind.Concrete.Sum where
import Data.Type.Coercion
import Data.Coerce
import Prelude hiding (id, (.))

type family (+) = (p :: k -> k -> k) | p -> k where
  (+) = Either
  (+) = Sum1
  (+) = Sum2
  (+) = Sum3
infixl 7 +

class CoercibleSum k where
  coercibleSum_ :: forall (a :: k) b (a' :: k) b'
                 . (Coercible a a', Coercible b b') => (a + b ~=~ a' + b')
  default coercibleSum_ :: forall (a :: k) b (a' :: k) b'
                         . Coercible (a + b) (a' + b') => (a + b ~=~ a' + b')
  coercibleSum_ = Coercion

(~+++~) :: forall (a :: k) b (a' :: k) b'
         . CoercibleSum k => (a ~=~ a') -> (b ~=~ b') -> (a + b ~=~ a' + b')
Coercion ~+++~ Coercion = coercibleSum_

class CoercibleSum k => WrapSum k where
  newtypeSum_ :: forall (a :: j -> k) b x. a x + b x ~=~ (a + b) x
  default newtypeSum_ :: forall (a :: j -> k) b x
                           . Coercible (a x + b x) ((a + b) x) => a x + b x ~=~ (a + b) x
  newtypeSum_ = Coercion

instance CoercibleSum *
instance WrapSum *
newtype Sum1 a b x = Sum1 { getSum1 :: Either (a x) (b x) }

instance CoercibleSum (k -> *)
instance WrapSum (k -> *)
newtype Sum2 a b x y = Sum2 { getSum2 :: Sum1 (a x) (b x) y }

instance CoercibleSum (j -> k -> *)
instance WrapSum (j -> k -> *)
newtype Sum3 a b x y z = Sum3 { getSum3 :: Sum2 (a x) (b x) y z }

instance CoercibleSum (i -> j -> k -> *)
