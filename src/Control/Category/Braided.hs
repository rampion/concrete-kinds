{-# MultiParamTypeClasses #-}
{-# TypeOperators #-}
{-# PolyKinds #-}
module Braided where

import Control.Category.Associative
import qualified Data.Tuple as Shadowed (swap)

-- the bifunctor operator is symmetric over the arrow
--
-- polykinded version of Control.Categorical.Braided.Braided from categories
class Associative arrow bifunctor => Braided arrow bifunctor where
  -- law: associate . braid . associate = second braid . associate . first braid
  -- law: disassociate . braid . disassociate = first braid . disassociate . second braid
  -- law: idr . braid = idl
  -- law: idl . braid = idr
  -- law: braid . coidr = coidl
  -- law: braid . coidl = coidr
  braid :: bifunctor a b `arrow` bifunctor b a

instance Braided (->) (,) where
  braid = swap

instance Braided (->) Either where
  braid = either Right Left

-- instance Braided arrow bifunctor => Braided (Dual arrow) bifunctor

-- polykinded version of Control.Categorical.Braided.Symmetric from categories
class Braided arrow bifunctor => Symmetric arrow bifunctor
  -- law: swap . swap = id

instance Symmetric (->) (,) where
instance Symmetric (->) Either
-- instance Symmetric arrow bifunctor => Symmetric (Dual arrow) bifunctor

swap :: Symmetric arrow bifunctor => bifunctor a b `arrow` bifunctor b a
swap = braid
