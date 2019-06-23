{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Choice where
import Control.Category
import qualified Control.Arrow as Shadowed

class Category m => Choice (m :: k -> k -> *) where
  type Coproduct m :: k -> k -> k
  left :: a `m` b -> Coproduct m a c `m` Coproduct m b c
  right :: a `m` b -> Coproduct m c a `m` Coproduct m c b
  (+++) :: a `m` a' -> b `m` b' -> Coproduct m a b `m` Coproduct m a' b'
  infixr 2 +++

instance Choice (->) where
  type Coproduct (->) = Either
  left = Shadowed.left
  right = Shadowed.right
  (+++) = (Shadowed.+++)

