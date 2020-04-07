{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Kind.Concrete.Extension where

-- Unit, Void, Identity, Subst
data family Unary (base :: * -> *) (a :: j -> k) :: j -> k
newtype instance Unary base a x     = Unary1 { getUnary1 :: base (a x) }
newtype instance Unary base a x y   = Unary2 { getUnary2 :: base (a x y) }
newtype instance Unary base a x y z = Unary3 { getUnary3 :: base (a x y z) }

-- Const, (×), (+) 
data family Binary (base :: * -> * -> *) (a :: j -> k) (b :: j -> k) :: j -> k
newtype instance Binary base a b x      = Binary1 { getBinary1 :: a x `base` b x }
newtype instance Binary base a b x y    = Binary2 { getBinary2 :: a x y `base` b x y }
newtype instance Binary base a b x y z  = Binary3 { getBinary3 :: a x y z `base` b x y z }

{-
data family Binary (base :: i -> j -> *) (a :: x -> i') (b :: y -> j') :: (x,y) -> k

-- newtype Identity_ a {b..z} = Identity_ { runIdentity_ :: a {b..z} }
data family Id (a :: k) :: k
newtype instance Id a      = BaseCase { runBaseCase :: a }
newtype instance Id a b .. = Recursion { runRecursion :: Id (a b) .. }

data    instance Unit       = BaseCase
newtype instance Unit a ..  = Recursion { runRecursion :: Unit .. }

data family Void :: k
data    instance Void

newtype Subst :: (i -> j -> k) -> (i -> j) -> i -> k
newtype Subst g f a .. = Subst { runSubst :: Id (g a (f a)) }

newtype Const :: k -> j -> k
newtype Const a b .. = Const { runConst :: Id a .. }
-}
