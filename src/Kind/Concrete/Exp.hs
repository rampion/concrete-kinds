{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Kind.Concrete.Exp where
import Data.Dual
import Data.Type.Coercion
import Data.Coerce
import Control.Category
import Prelude hiding (id, (.))

type family (^) = (e :: k -> k -> k) | e -> k where
  (^) = Dual (->)
  (^) = Exp_

class WrapExp k where
  data Exp_ :: (j -> k) -> (j -> k) -> j -> k

  newtypeExp_ :: forall (a :: j -> k) b x. a x ^ b x ~=~ (a ^ b) x
  default newtypeExp_ :: forall (a :: j -> k) b x
                       . Coercible (a x ^ b x) ((a ^ b) x)
                      => a x ^ b x ~=~ (a ^ b) x
  newtypeExp_ = Coercion

  app :: forall (a :: k) (a' :: k) (b :: k) (b' :: k)
       . (a ~=~ a') -> (b ~=~ b') -> (a ^ b ~=~ a' ^ b')
  default app :: forall (a :: k) b (a' :: k) b' j' k'
               . (k ~ (j' -> k'), WrapExp k')
              => (a ~=~ a') -> (b ~=~ b') -> (a ^ b ~=~ a' ^ b')
  app = coercibleExp_

coercibleExp_ :: forall (a :: j -> k) b (a' :: j -> k) b'
               . WrapExp k => (a ~=~ a') -> (b ~=~ b') -> (a ^ b ~=~ a' ^ b')
coercibleExp_ Coercion Coercion = eliminate (newtypeExp_ . app Coercion Coercion . sym newtypeExp_)

instance WrapExp * where
  newtype Exp_ a b x = Exp1 { getExp1 :: a x ^ b x }
  app Coercion Coercion = Coercion

instance WrapExp (j -> *) where
  newtype Exp_ a b x y = Exp2 { getExp2 :: (a x ^ b x) y }

instance WrapExp (i -> j -> *) where
  newtype Exp_ a b x y z = Exp3 { getExp3 :: (a x ^ b x) y z }
