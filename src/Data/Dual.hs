{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Dual where
import Control.Category
import Control.Category.Strong
import Control.Category.Choice
import Control.Category.Cartesian
import Control.Category.Cocartesian
import Control.Category.Final
import Control.Category.Initial
import Data.Iso
import Prelude hiding (id,(.),fst,snd)

newtype Dual m a b = Dual { runDual :: m b a }

type family Dual' (m :: k -> k -> *) = (m' :: k -> k -> *) where -- | m' -> m where
  Dual' (Dual m) = m
  Dual' m = Dual m

_Dual :: (m a b <-> Dual m b a)
_Dual = Iso Dual runDual

instance Category m => Category (Dual m) where
  id = Dual id
  Dual f . Dual g = Dual (g . f)

instance Choice m => Strong (Dual m) where
  type Product (Dual m) = Coproduct m
  first (Dual f) = Dual (left f)
  second (Dual f) = Dual (right f)
  Dual f *** Dual g = Dual (f +++ g)

instance Cocartesian m => Cartesian (Dual m) where
  fst = Dual inl
  snd = Dual inr
  Dual f &&&  Dual g = Dual (f ||| g)

instance Strong m => Choice (Dual m) where
  type Coproduct (Dual m) = Product m
  left (Dual f) = Dual (first f)
  right (Dual f) = Dual (second f)
  Dual f +++ Dual g = Dual (f *** g)

instance Cartesian m => Cocartesian (Dual m) where
  inl = Dual fst
  inr = Dual snd
  Dual f ||| Dual g = Dual (f &&& g)
  
instance Initial m => Final (Dual m) where
  type Terminal (Dual m) = Coterminal m
  unit = Dual absurd

instance Final m => Initial (Dual m) where
  absurd = Dual unit
  type Coterminal (Dual m) = Terminal m
