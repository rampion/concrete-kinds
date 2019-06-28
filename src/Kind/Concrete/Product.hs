{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Product where
import Data.ReprEq
import Data.Coerce
import Control.Category
import Prelude hiding (id, (.))

type family (×) = (p :: k -> k -> k) | p -> k where
  (×) = (,)
  (×) = Product_
infixl 7 ×

class Product_ ~ (×) => WrapProduct k where
  data Product_ :: (j -> k) -> (j -> k) -> j -> k

  newtypeProduct_ :: forall (a :: j -> k) b x. a x × b x ~=~ (a × b) x
  default newtypeProduct_ :: forall (a :: j -> k) b x
                           . Coercible (a x × b x) ((a × b) x) => a x × b x ~=~ (a × b) x
  newtypeProduct_ = IsCoercible

  infixr 3 ~***~
  (~***~) :: forall (a :: k) b (a' :: k) b'
           . (a ~=~ a') -> (b ~=~ b') -> (a × b ~=~ a' × b')
  default (~***~) :: forall (a :: k) b (a' :: k) b' j' k'
                   . (k ~ (j' -> k'), WrapProduct k')
                  => (a ~=~ a') -> (b ~=~ b') -> (a × b ~=~ a' × b')
  (~***~) = coercibleProduct_

coercibleProduct_ :: forall (a :: j -> k) b (a' :: j -> k) b'
         . WrapProduct k => (a ~=~ a') -> (b ~=~ b') -> (a × b ~=~ a' × b')
coercibleProduct_ IsCoercible IsCoercible = eliminate (newtypeProduct_ . (IsCoercible ~***~ IsCoercible) . sym newtypeProduct_)

instance WrapProduct * where
  newtype Product_ a b x = Product1 { getProduct1 :: a x × b x }
  IsCoercible ~***~ IsCoercible = IsCoercible

instance WrapProduct (k -> *) where
  newtype Product_ a b x y = Product2 { getProduct2 :: (a x × b x) y }

instance WrapProduct (j -> k -> *) where
  newtype Product_ a b x y z = Product3 { getProduct3 :: (a x × b x) y z }
