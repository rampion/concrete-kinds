{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Closed where
import Control.Category
import Control.Category.Strong
import Data.Iso
import Prelude hiding ((.),id)

class Strong m => Closed (m :: k -> k -> *) where
  type Exp m :: k -> k -> k
  _Curry :: ((Product m a b) `m` c) <-> (a `m` Exp m c b)

eval :: Closed m => Product m (Exp m b a) a `m` b
eval = from _Curry id

instance Closed (->) where
  type Exp (->) = Op
  _Curry = Iso
    (\f -> Op . curry f)
    (\f -> uncurry (runOp . f))

newtype Op a b = Op { runOp :: b -> a }
