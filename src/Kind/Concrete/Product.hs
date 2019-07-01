{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Product where
import Data.Type.Coercion
import Data.Coerce
import Prelude hiding (id, (.))

type family (×) = (p :: k -> k -> k) | p -> k where
  (×) = (,)
  (×) = Product1
  (×) = Product2
  (×) = Product3
infixl 7 ×

class CoercibleProduct k where
  coercibleProduct_ :: forall (a :: k) b (a' :: k) b'
                     . (Coercible a a', Coercible b b')
                    => (a × b ~=~ a' × b')
  default coercibleProduct_ :: forall (a :: k) b (a' :: k) b'
                             . Coercible (a × b) (a' × b')
                            => (a × b ~=~ a' × b')
  coercibleProduct_ = Coercion

infixr 3 ~***~
(~***~) :: forall (a :: k) b (a' :: k) b'
         . CoercibleProduct k => (a ~=~ a') -> (b ~=~ b') -> (a × b ~=~ a' × b')
Coercion ~***~ Coercion = coercibleProduct_

class CoercibleProduct k => WrapProduct k where
  newtypeProduct_ :: forall (a :: j -> k) b x. a x × b x ~=~ (a × b) x
  default newtypeProduct_ :: forall (a :: j -> k) b x
                           . Coercible (a x × b x) ((a × b) x) => a x × b x ~=~ (a × b) x
  newtypeProduct_ = Coercion

instance CoercibleProduct *
instance WrapProduct *
newtype Product1 a b x = Product1 { getProduct1 :: (a x, b x) }

instance CoercibleProduct (k -> *)
instance WrapProduct (k -> *)
newtype Product2 a b x y = Product2 { getProduct2 :: Product1 (a x) (b x) y }

instance CoercibleProduct (j -> k -> *)
instance WrapProduct (j -> k -> *)
newtype Product3 a b x y z = Product3 { getProduct3 :: Product2 (a x) (b x) y z }

instance CoercibleProduct (i -> j -> k -> *)
