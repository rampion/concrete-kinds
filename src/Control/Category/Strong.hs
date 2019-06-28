{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Strong where
import Control.Category
import qualified Control.Arrow as Shadowed
import Prelude hiding ((.), id)

class Category m => Strong (m :: k -> k -> *) where
  type Product m :: k -> k -> k

  first :: a `m` b -> Product m a c `m` Product m b c
  first = (*** id)

  second :: a `m` b -> Product m c a `m` Product m c b
  second = (id ***)

  infixr 3 ***
  (***) :: a `m` a' -> b `m` b' -> Product m a b `m` Product m a' b'
  f *** g = first f . second g

  {-# MINIMAL (***) | first, second #-}

instance Strong (->) where
  type Product (->) = (,)
  first = Shadowed.first
  second = Shadowed.second
  (***) = (Shadowed.***)
