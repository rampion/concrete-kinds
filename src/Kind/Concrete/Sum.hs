{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Sum where
import Data.ReprEq
import Data.Coerce
import Control.Category
import Prelude hiding (id, (.))

type family (+) = (p :: k -> k -> k) | p -> k where
  (+) = Either
  (+) = Sum_
infixl 7 +

class Sum_ ~ (+) => WrapSum k where
  data Sum_ :: (j -> k) -> (j -> k) -> j -> k

  newtypeSum_ :: forall (a :: j -> k) b x. a x + b x ~=~ (a + b) x
  default newtypeSum_ :: forall (a :: j -> k) b x
                           . Coercible (a x + b x) ((a + b) x) => a x + b x ~=~ (a + b) x
  newtypeSum_ = IsCoercible

  coercibleSum_ :: forall (a :: j -> k) b (a' :: j -> k) b'
                     . (Coercible a a', Coercible b b') => (a + b ~=~ a' + b')
  default coercibleSum_ :: forall (a :: j -> k) b (a' :: j -> k) b' j' k'
                             . (Coercible a a', Coercible b b', k ~ (j' -> k'), WrapSum k')
                            => (a + b ~=~ a' + b')
  coercibleSum_ = eliminate (newtypeSum_ . coercibleSum_ . sym newtypeSum_)

instance WrapSum * where
  newtype Sum_ a b x = Sum1 { getSum1 :: a x + b x }
  coercibleSum_ = eliminate (newtypeSum_ . IsCoercible . sym newtypeSum_)

instance WrapSum (k -> *) where
  newtype Sum_ a b x y = Sum2 { getSum2 :: (a x + b x) y }

instance WrapSum (j -> k -> *) where
  newtype Sum_ a b x y z = Sum3 { getSum3 :: (a x + b x) y z }
