{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Kind.Concrete.Identity where
import qualified Data.Functor.Identity as Shadowed
import Kind.Concrete.Extension as Extension
import Data.Coerce

import Kind.Concrete

type family Identity = (i :: k -> k) | i -> k where
  Identity = Shadowed.Identity
  Identity = Extension.Unary Shadowed.Identity

type HasIdentity k = forall (a :: k). Coercible a (Identity a)

_Identity :: (HasIdentity k, Concrete k) => forall (a :: k). a <~> _Identity a
_Identity = _Coerce

{-
instance HasIdentity * where
  _Identity = Iso Shadowed.Identity Shadowed.runIdentity

instance HasIdentity k => HasIdentity (j -> k) where
  _Identity = _
--

{-
data family Identity_ (a :: j -> k) :: j -> k
newtype instance Identity_ a x = Identity1 { runIdentity1 :: a x }
newtype instance Identity_ a x y = Identity2 { runIdentity2 :: a x y }
newtype instance Identity_ a x y z = Identity3 { runIdentity3 :: a x y z }
-}
-}
