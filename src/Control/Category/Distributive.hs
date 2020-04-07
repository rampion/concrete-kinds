{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
module Control.Category.Distributive where
import Control.Category.Strong
import Control.Category.Choice
import Control.Category.Cocartesian

class (Strong m, Choice m) => Distributive (m :: k -> k -> *) where
  distribute :: Product m c (Coproduct m a b) `m` Coproduct m (Product m c a) (Product m c b)

instance Distributive (->) where
  distribute = uncurry $ \c -> (c,) +++ (c,)

codistribute :: (Strong m, Cocartesian m) => Coproduct m (Product m c a) (Product m c b) `m` Product m c (Coproduct m a b)
codistribute = second inl ||| second inr
