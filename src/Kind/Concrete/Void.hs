{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Kind.Concrete.Void where
import qualified Data.Void as Shadowed

type family Void = (v :: k) | v -> k where
  Void = Shadowed.Void
  Void = Void_

data family Void_ :: (i -> k)
data instance Void_ x
data instance Void_ x y
data instance Void_ x y z
