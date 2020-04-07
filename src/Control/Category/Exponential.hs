{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RankNTypes #-}
module Control.Category.Exponential where
import Control.Category
import Control.Category.Strong
import Data.Iso
import Prelude hiding ((.))

class Category m => Exponential (m :: k -> k -> *) where
  type Exp m :: k -> k -> k
  _Curry :: ((Product m a b) `m` c) <-> (a `m` Exp m c b)

instance Exponential (->) where
  type Exp (->) = Dual (->)
  _Curry = Iso
    (\f -> Dual . curry f)
    (\f -> uncurry (runDual . f))

-- newtype Op a b = Op { runOp :: b -> a }
-- stolen from categories
newtype Dual k a b = Dual { runDual :: k b a }

-- https://stackoverflow.com/questions/58041216/generalizing-extending-a-type-by-a-data-family
type family (^) = (p :: k -> k -> k) | p -> k where
  (^) = Dual (->)
  (^) = Extension_ (^)
infixr 8 ^

{-
data family Exp_ (a :: j -> k) (b :: j -> k) :: j -> k


type family Extension k (base :: * -> * -> *) = (p :: k -> k -> k) | p -> k where
  Extension * base = base 
  Extension (j -> k) base = Extension_ base
-}

data family Extension_ (base :: * -> * -> *) (a :: j -> k) (b :: j -> k) :: j -> k
newtype instance Extension_ base a b x = Extension1 { getExtension1 :: a x `base` b x }
newtype instance Extension_ base a b x y = Extension2 { getExtension2 :: a x y `base` b x y }
newtype instance Extension_ base a b x y z = Extension3 { getExtension3 :: a x y z `base` b x y z }

{-
type (+) :: k -> k -> k 
type (+) = Extension k Either
type ((×) :: k -> k -> k) = Extension k (,)
type ((^) :: k -> k -> k) = Extension k (Dual (->))
-}
-- ...
{-
src/Extension.hs:36:3: error:
    • Type family equation violates injectivity annotation.
      RHS of injective type family equation is a bare type variable
      but these LHS type and kind patterns are not bare variables: ‘*’
        Extension base = base
          -- Defined at src/Control/Category/Exponential.hs:36:3
    • In the equations for closed type family ‘Extension’
      In the type family declaration for ‘Extension’
   |
36 |   Extension base = base
   |   ^^^^^^^^^^^^^^^^^^^^^

src/Extension.hs:36:3: error:
    • Type family equations violate injectivity annotation:
        Extension base = base
          -- Defined at src/Control/Category/Exponential.hs:36:3
        Extension base = Extension_ base
          -- Defined at src/Control/Category/Exponential.hs:37:3
    • In the equations for closed type family ‘Extension’
      In the type family declaration for ‘Extension’
   |
36 |   Extension base = base
   |   ^^^^^^^^^^^^^^^^^^^^^
 
-}
