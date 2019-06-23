{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Iso where
import Control.Category
import Control.Category.Strong
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Prelude hiding (id, (.), fst, snd)

data Iso m a b = Iso { to :: m a b, from :: m b a }

rev :: Iso m a b -> Iso m b a
rev (Iso f f') = Iso f' f

instance Category m => Category (Iso m) where
  id = Iso id id
  Iso f f' . Iso g g' = Iso (f . g) (g' . f')

type (<->) = Iso (->)
infix 0 <->

instance Strong m => Strong (Iso m) where
  type Product (Iso m) = Product m
  first (Iso f f') = Iso (first f) (first f')
  second (Iso f f') = Iso (second f) (second f')
  Iso f f' *** Iso g g' = Iso (f *** g) (f' *** g')

_Product :: Cartesian m => (c `m` a, c `m` b) <-> (c `m` Product m a b)
_Product = Iso (uncurry (&&&)) ((fst.) &&& (snd.))

instance Choice m => Choice (Iso m) where
  type Coproduct (Iso m) = Coproduct m
  left (Iso f f') = Iso (left f) (left f')
  right (Iso f f') = Iso (right f) (right f')
  Iso f f' +++ Iso g g' = Iso (f +++ g) (f' +++ g')

_Sum :: Cocartesian m => (a `m` c, b `m` c) <-> (Coproduct m a b `m` c)
_Sum = Iso (uncurry (|||)) ((.inl) &&& (.inr))
