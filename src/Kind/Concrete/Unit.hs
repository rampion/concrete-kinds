{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Kind.Concrete.Unit where
import Data.Proxy (Proxy(..))

type family Unit = (u :: k) | u -> k where
  Unit = ()
  Unit = Proxy
  Unit = Unit_

data family Unit_ :: (i -> j -> k)
data instance Unit_ x y = Unit2
data instance Unit_ x y z = Unit3
