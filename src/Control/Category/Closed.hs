{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Closed where
import Control.Category
import Control.Category.Strong
import Data.Iso
import Data.Dual
import Prelude hiding ((.),id)

class Strong m => Closed (m :: k -> k -> *) where
  type Exp m :: k -> k -> k
  _Curry :: (Product m a b `m` c) <-> (a `m` Exp m c b)

eval :: Closed m => Product m (Exp m b a) a `m` b
eval = from _Curry id

instance Closed (->) where
  type Exp (->) = Dual (->)
  _Curry = Iso
    (\f -> Dual . curry f)
    (\f -> uncurry (runDual . f))
