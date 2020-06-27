{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module Data.Universal where
import Prelude hiding (id, (.), fst, snd, Functor, fmap)
import Control.Category

newtype Universal (op :: k -> k -> *) (a :: j -> k) (b :: j -> k) 
  = Universal { getUniversal :: forall x. a x `op` b x }

instance Category arrow => Category (Universal arrow) where
  id = Universal id
  Universal p . Universal q = Universal (p . q)

