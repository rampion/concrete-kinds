{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Dual where
import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion
import Data.Functor.Contravariant
import Data.Bifunctor

class HasDual k where
  data Dual :: forall i j. (i -> j -> k) -> j -> i -> k
  newtypeDual :: forall (op :: i -> j -> k) a b. Dual op a b `Coercion` op b a

instance HasDual * where
  newtype Dual op a b = Dual1 { getDual1 :: op b a }
  newtypeDual = Coercion

instance HasDual (k -> *) where
  newtype Dual op a b x = Dual2 { getDual2 :: op b a x }
  newtypeDual = Coercion

instance Category arrow => Category (Dual arrow) where
  id = Dual1 id
  Dual1 f . Dual1 g = Dual1 (g . f)

type family Flipped (op :: i -> j -> k) :: j -> i -> k where
  Flipped (Dual op) = op
  Flipped op = Dual op

class HasDual k => Flippable (op :: i -> j -> k) where
  newtypeFlipped :: Flipped op a b `Coercion` op b a
  involutionFlipped :: Flipped (Flipped op) `Coercion` op

instance {-# OVERLAPPABLE #-} (HasDual k, Flipped op ~ Dual op) => Flippable (op :: i -> j -> k) where
  newtypeFlipped = newtypeDual
  involutionFlipped = Coercion

instance (HasDual k, Flipped op ~ Dual op) => Flippable (Dual op :: i -> j -> k) where
  newtypeFlipped = sym newtypeDual
  involutionFlipped = Coercion

instance Contravariant ((->) `Dual` x) where
  contramap f (Dual1 g) = Dual1 (g . f)

instance Bifunctor bifunctor => Bifunctor (Dual bifunctor) where
  first g (Dual1 fxa) = Dual1 (second g fxa)
  second g (Dual1 fax) = Dual1 (first g fax)
  
