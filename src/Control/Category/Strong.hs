{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Strong where
import Control.Category
import qualified Control.Arrow as Shadowed

-- idl :: (Id arrow p `op` a) `arrow` a 
--
-- Terminal arrow ~ Id arrow product  => CartesianClosed
-- Initial arrow ~ Id arrow coproduct


class Category arrow => Strong (arrow :: k -> k -> *) where
  type Product arrow :: k -> k -> k
  first :: a `arrow` b -> Product arrow a c `arrow` Product arrow b c
  second :: a `arrow` b -> Product arrow c a `arrow` Product arrow c b
  (***) :: a `arrow` a' -> b `arrow` b' -> Product arrow a b `arrow` Product arrow a' b'
  infixr 3 ***

instance Strong (->) where
  first = Shadowed.first
  second = Shadowed.second
  (***) = (Shadowed.***)
