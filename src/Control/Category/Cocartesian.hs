{-# LANGUAGE TypeOperators #-}
module Control.Category.Cocartesian where
import Control.Category.Choice

class Choice m => Cocartesian m where
  inl :: a `m` Coproduct m a b
  inr :: b `m` Coproduct m a b
  (|||) :: a `m` c -> b `m` c -> Coproduct m a b `m` c
  infixr 2 |||

instance Cocartesian (->) where
  inl = Left
  inr = Right
  (|||) = either
