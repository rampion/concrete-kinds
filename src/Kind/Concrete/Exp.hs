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
import Data.ReprEq
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
  newtypeExp_ = IsCoercible

  coercibleExp_ :: forall (a :: j -> k) (a' :: j -> k) (b :: j -> k) (b' :: j -> k)
                 . (Coercible a a', Coercible b b') => (a ^ b) ~=~ (a' ^ b')
  default coercibleExp_ :: forall (a :: j -> k) b (a' :: j -> k) b' j' k'
                         . (Coercible a a', Coercible b b', k ~ (j' -> k'), WrapExp k')
                        => (a ^ b ~=~ a' ^ b')
  coercibleExp_ = eliminate (newtypeExp_ . coercibleExp_ . sym newtypeExp_)
  

instance WrapExp * where
  newtype Exp_ a b x = Exp1 { getExp1 :: a x ^ b x }
  coercibleExp_ = eliminate (newtypeExp_ . IsCoercible . sym newtypeExp_)

instance WrapExp (j -> *) where
  newtype Exp_ a b x y = Exp2 { getExp2 :: (a x ^ b x) y }

instance WrapExp (i -> j -> *) where
  newtype Exp_ a b x y z = Exp3 { getExp3 :: (a x ^ b x) y z }
