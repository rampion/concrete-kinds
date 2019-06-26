{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Fix where
import Kind.Concrete.Natural
import Control.Category.Coercible

data family Fix (f :: k -> k) :: k
newtype instance Fix f = Fix { unFix :: f (Fix f) }
newtype instance Fix f x = Fix1 { unFix1 :: f (Fix f) x }
newtype instance Fix f x y = Fix2 { unFix2 :: f (Fix f) x y }
newtype instance Fix f x y z = Fix3 { unFix3 :: f (Fix f) x y z }

_Fix :: f (Fix f) <~> Fix f
_Fix = coerce
