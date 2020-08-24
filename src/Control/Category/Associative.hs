{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Category.Associative where

import Control.Categorical.Bifunctor
import Data.Dual

-- | The bifunctor has an associativity law over the arrow
-- 
-- Laws:
--  associate . disassociate = id
--  disassociate . associate = id
--  associate . associate = second associate . associate . first associate
--  disassociate . disassociate = first disassociate . disassociate . second disassociate
class Bifunctor bifunctor arrow arrow arrow => Associative arrow bifunctor where
  associate :: ((a `bifunctor` b) `bifunctor` c) `arrow` (a `bifunctor` (b `bifunctor` c))
  disassociate :: (a `bifunctor` (b `bifunctor` c)) `arrow` ((a `bifunctor` b) `bifunctor` c)

instance Associative (->) (,) where
  associate ((a,b),c) = (a,(b,c))
  disassociate (a,(b,c)) = ((a,b),c)

instance Associative (->) Either where
  associate = (Left `either` (Right . Left)) `either` (Right . Right)
  disassociate = (Left . Left) `either` ((Left . Right) `either` Right)

instance 
    ( Associative arrow bifunctor
    , Flipped arrow ~ Dual arrow
    ) => Associative (Dual arrow) bifunctor where
  associate = Dual1 disassociate
  disassociate = Dual1 associate

example :: ((a,b),c) -> (a,(b,c))
example = associate

dual_example :: ((x,y),z) -> (x,(y,z))
dual_example = getDual1 disassociate
