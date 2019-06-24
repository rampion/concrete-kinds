{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Exponential where
import Control.Category
import Control.Category.Strong
import Data.Iso
import Prelude hiding ((.))

class Category m => Exponential (m :: k -> k -> *) where
  type Exp m :: k -> k -> k
  _Curry :: ((Product m a b) `m` c) <-> (a `m` Exp m c b)

instance Exponential (->) where
  type Exp (->) = Op
  _Curry = Iso
    (\f -> Op . curry f)
    (\f -> uncurry (runOp . f))

newtype Op a b = Op { runOp :: b -> a }
