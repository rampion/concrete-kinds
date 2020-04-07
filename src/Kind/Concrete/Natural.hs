{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Kind.Concrete.Natural 
  ( type (~>)
  , Natural_(..)
  , pattern Natural2, runNatural2
  , pattern Natural3, runNatural3
  ) where

type family (~>) = (m :: k -> k -> *) | m -> k where
  (~>) = (->)
  (~>) = Natural_
infixr 0 ~>
newtype Natural_ (a :: j -> k) (b :: j -> k) 
  = Natural1 { runNatural1 :: forall (x :: j). a x ~> b x }

-- TODO: get view pattern without intermediate type
pattern Natural2 :: (forall (x :: i) (y :: j). a x y ~> b x y) -> Natural_ a b
pattern Natural2 { runNatural2 } <- (toNatural2' -> runNatural2' -> runNatural2) -- yes
-- pattern Natural2 { runNatural2 } <- (runNatural1 -> runNatural1 -> runNatural2) -- no
-- pattern Natural2 { runNatural2 } <- (runNatural1 -> Natural1 runNatural2) -- no
-- pattern Natural2 { runNatural2 } <- Natural1 (runNatural1 -> runNatural2) -- no
  where Natural2 f = Natural1 (Natural1 f)
{-# COMPLETE Natural2 #-}

newtype Natural2' (a :: i -> j -> k) (b :: i -> j -> k) 
  = Natural2' { runNatural2' :: forall (x :: i) (y :: j). a x y ~> b x y }

toNatural2' :: Natural_ a b -> Natural2' a b
-- toNatural2' (Natural1 f) = Natural2' (runNatural1 f) -- yes
toNatural2' f = Natural2' (runNatural1 (runNatural1 f)) -- yes

newtype Natural3' (a :: h -> i -> j -> k) (b :: h -> i -> j -> k)
  = Natural3' { runNatural3' :: forall (x :: h) (y :: i) (z :: j). a x y z ~> b x y z }

toNatural3' :: Natural_ a b -> Natural3' a b
-- toNatural3' (Natural2 f) = Natural3' (runNatural1 f) -- yes
-- toNatural3' (Natural1 f) = Natural3' (runNatural2 f) -- yes
-- toNatural3' (Natural1 f) = Natural3' (runNatural1 (runNatural1 f)) -- yes
-- toNatural3' (Natural1 (Natural1 f)) = Natural3' (runNatural1 f) -- no
-- toNatural3' (Natural1 (runNatural1 -> f)) = Natural3' (runNatural1 f) -- no
toNatural3' f = Natural3' (runNatural1 (runNatural1 (runNatural1 f))) -- yes

pattern Natural3 :: (forall (x :: h) (y :: i) (z :: j). a x y z ~> b x y z) -> Natural_ a b
pattern Natural3 { runNatural3 } <- (toNatural3' -> runNatural3' -> runNatural3)
  where Natural3 f = Natural1 (Natural2 f)
{-# COMPLETE Natural3 #-}
