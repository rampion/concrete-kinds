{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Compose where
import Kind.Concrete.Natural
import Control.Category.Coercible
import Data.Functor.Compose

type family (∘) = (o :: (j -> k) -> (i -> j) -> i -> k) | o -> k where
  (∘) = Compose
  (∘) = Compose_
infixr 9 ∘

data family Compose_ (f :: i -> j -> k) (g :: h -> i) (a :: h) :: j -> k
newtype instance Compose_ f g a x = Compose1 { getCompose1 :: f (g a) x }
newtype instance Compose_ f g a x y = Compose2 { getCompose2 :: f (g a) x y }
newtype instance Compose_ f g a x y z = Compose3 { getCompose3 :: f (g a) x y z }

_Compose :: f (g a) <~> (f ∘ g) a
_Compose = coerce
