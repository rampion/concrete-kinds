{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Associative where

import Control.Categorical.Bifunctor

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

