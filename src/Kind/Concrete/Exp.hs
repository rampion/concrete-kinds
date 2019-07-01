{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Kind.Concrete.Exp where
import Data.Type.Coercion
import Data.Coerce
import Control.Category.Closed

type family (^) = (e :: k -> k -> k) | e -> k where
  (^) = ReversedFunction
  (^) = Exp1
  (^) = Exp2
  (^) = Exp3

class CoercibleExp k where
  coercibleExp_ :: forall (a :: k) b (a' :: k) b'
                 . (Coercible a a', Coercible b b')
                => (a ^ b ~=~ a' ^ b')
  default coercibleExp_ :: forall (a :: k) b (a' :: k) b'
                         . Coercible (a ^ b) (a' ^ b')
                        => (a ^ b ~=~ a' ^ b')
  coercibleExp_ = Coercion

app :: forall (a :: k) (a' :: k) (b :: k) (b' :: k)
     . CoercibleExp k => (a ~=~ a') -> (b ~=~ b') -> (a ^ b ~=~ a' ^ b')
app Coercion Coercion = coercibleExp_

class WrapExp k where
  newtypeExp_ :: forall (a :: j -> k) b x. a x ^ b x ~=~ (a ^ b) x
  default newtypeExp_ :: forall (a :: j -> k) b x
                       . Coercible (a x ^ b x) ((a ^ b) x)
                      => a x ^ b x ~=~ (a ^ b) x
  newtypeExp_ = Coercion

instance CoercibleExp *
instance WrapExp *
newtype Exp1 a b x = Exp1 { getExp1 :: ReversedFunction (a x) (b x) }

instance CoercibleExp (j -> *)
instance WrapExp (j -> *)
newtype Exp2 a b x y = Exp2 { getExp2 :: Exp1 (a x) (b x) y }

instance CoercibleExp (i -> j -> *)
instance WrapExp (i -> j -> *)
newtype Exp3 a b x y z = Exp3 { getExp3 :: Exp2 (a x) (b x) y z }

instance CoercibleExp (i -> j -> k -> *)
