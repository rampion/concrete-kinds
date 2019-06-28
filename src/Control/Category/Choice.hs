{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Choice where
import Prelude hiding (id, (.))
import Control.Category
import qualified Control.Arrow as Shadowed

class Category m => Choice (m :: k -> k -> *) where
  type Coproduct m :: k -> k -> k

  left :: a `m` b -> Coproduct m a c `m` Coproduct m b c
  left = (+++id)

  right :: a `m` b -> Coproduct m c a `m` Coproduct m c b
  right = (id+++)

  infixr 2 +++
  (+++) :: a `m` a' -> b `m` b' -> Coproduct m a b `m` Coproduct m a' b'
  f +++ g = left f . right g

  {-# MINIMAL left,right | (+++) #-}

instance Choice (->) where
  type Coproduct (->) = Either
  left = Shadowed.left
  right = Shadowed.right
  (+++) = (Shadowed.+++)

