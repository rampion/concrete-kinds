{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Strong where
import Control.Category
import qualified Control.Arrow as Shadowed

class Category m => Strong (m :: k -> k -> *) where
  type Product m :: k -> k -> k
  first :: a `m` b -> Product m a c `m` Product m b c
  second :: a `m` b -> Product m c a `m` Product m c b
  (***) :: a `m` a' -> b `m` b' -> Product m a b `m` Product m a' b'
  infixr 3 ***

instance Strong (->) where
  type Product (->) = (,)
  first = Shadowed.first
  second = Shadowed.second
  (***) = (Shadowed.***)
