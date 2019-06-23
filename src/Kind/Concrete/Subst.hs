{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Kind.Concrete.Subst where

data family Subst :: (i -> j -> k) -> (i -> j) -> i -> k
newtype instance Subst a b c = Subst { getSubst :: a c (b c) }
newtype instance Subst a b c x = Subst1 { getSubst1 :: a c (b c) x }
newtype instance Subst a b c x y = Subst2 { getSubst2 :: a c (b c) x y }
newtype instance Subst a b c x y z = Subst3 { getSubst3 :: a c (b c) x y z }
