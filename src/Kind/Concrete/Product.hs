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

-- TODO: Rename - WrapProduct k says (j -> k) has a product.  Maybe WrapProduct?
class Product_ ~ (×) => WrapProduct k where
  data Product_ :: (j -> k) -> (j -> k) -> j -> k

  newtypeProduct_ :: forall (a :: j -> k) b x. a x × b x ~=~ (a × b) x
  default newtypeProduct_ :: forall (a :: j -> k) b x
                           . Coercible (a x × b x) ((a × b) x) => a x × b x ~=~ (a × b) x
  newtypeProduct_ = IsCoercible

  coercibleProduct_ :: forall (a :: j -> k) b (a' :: j -> k) b'
                     . (Coercible a a', Coercible b b') => (a × b ~=~ a' × b')
  default coercibleProduct_ :: forall (a :: j -> k) b (a' :: j -> k) b' j' k'
                             . (Coercible a a', Coercible b b', k ~ (j' -> k'), WrapProduct k')
                            => (a × b ~=~ a' × b')
  coercibleProduct_ = eliminate (newtypeProduct_ . coercibleProduct_ . sym newtypeProduct_)

instance WrapProduct * where
  newtype Product_ a b x = Product1 { getProduct1 :: a x × b x }
  coercibleProduct_ = eliminate (newtypeProduct_ . IsCoercible . sym newtypeProduct_)

instance WrapProduct (k -> *) where
  newtype Product_ a b x y = Product2 { getProduct2 :: (a x × b x) y }

instance WrapProduct (j -> k -> *) where
  newtype Product_ a b x y z = Product3 { getProduct3 :: (a x × b x) y z }
