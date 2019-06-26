{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Closed where
import Control.Category
import Control.Category.Strong
import Control.Category.Choice
import Data.Dual
import Data.Iso
import Prelude hiding ((.),id)

class Strong m => Closed (m :: k -> k -> *) where
  type Exp m :: k -> k -> k
  _Closure :: (Product m a b `m` c) <-> (a `m` Exp m c b)

eval :: Closed m => Product m (Exp m b a) a `m` b
eval = from _Closure id

instance Closed (->) where
  type Exp (->) = Dual (->)
  _Closure = fiso _Dual . _Curry

instance Coclosed m => Closed (Dual m) where
  type Exp (Dual m) = Coexp m
  _Closure = _Dual . _Coclosure . rev _Dual

class Choice m => Coclosed (m :: k -> k -> *) where
  type Coexp m :: k -> k -> k
  _Coclosure :: (c `m` Coproduct m a b) <-> (Coexp m c b `m` a)

assume :: Coclosed m => a `m` Coproduct m (Coexp m a b) b
assume = from _Coclosure id

instance Closed m => Coclosed (Dual m) where
  type Coexp (Dual m) = Exp m
  _Coclosure = _Dual . _Closure . rev _Dual
