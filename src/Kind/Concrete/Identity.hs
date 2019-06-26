{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Identity where
import Kind.Concrete.Natural
import Control.Category.Coercible
import qualified Data.Functor.Identity as Shadowed

type family Identity = (i :: k -> k) | i -> k where
  Identity = Shadowed.Identity
  Identity = Identity_

data family Identity_ (a :: j -> k) :: j -> k
newtype instance Identity_ a x = Identity1 { runIdentity1 :: a x }
newtype instance Identity_ a x y = Identity2 { runIdentity2 :: a x y }
newtype instance Identity_ a x y z = Identity3 { runIdentity3 :: a x y z }

_Identity :: a <~> Identity a
_Identity = coerce
