-- iabbrev >< ×
-- iabbrev o ∘
{-# OPTIONS_GHC -Wall -Wextra -Werror #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module PolyKindedData where

import GHC.Types (Constraint)
import Control.Category (Category(..))
import Data.Coerce (Coercible)

type (<~>) = Iso (~>)
infix 0 <~>

class Category m => Exponential (m :: k -> k -> *) where
  type Exp m :: k -> k -> k

data family (^) :: k -> k -> k

class ( Distributive ((~>) :: k -> k -> *) 
      , Cartesian ((~>) :: k -> k -> *) 
      , Cocartesian ((~>) :: k -> k -> *) 
      , Initial ((~>) :: k -> k -> *) 
      , Final ((~>) :: k -> k -> *) 
      , Exponential ((~>) :: k -> k -> *) 
      , Product ((~>) :: k -> k -> *) ~ (×)
      , Coproduct ((~>) :: k -> k -> *) ~ (+)
      , Terminal ((~>) :: k -> k -> *) ~ Unit
      , Coterminal ((~>) :: k -> k -> *) ~ Void
      , Exp ((~>) :: k -> k -> *) ~ (^)
      ) => Concrete k where

  _Coerce :: forall (a :: k) b. Coercible a b => a <~> b

  -- FIXME: can't get the kind of Data.Functor.Compose to line up
  data (∘) :: forall i j. (j -> k) -> (i -> j) -> i -> k
  infixr 9 ∘

  data (^) :: forall. k -> k -> k
  infixr 8 ^
  _Curry :: forall (a :: k) b c. ((a × b) ~> c) <-> (a ~> c ^ b)

  data Free :: (k -> Constraint) -> k -> k
  call :: forall (a :: k) (c :: k -> Constraint). a ~> Free c a
  interpret :: forall (a :: k) b (c :: k -> Constraint). c b => (a ~> b) -> Free c a ~> b

  data Fix :: (k -> k) -> k

_Identity :: forall (a :: k). (Concrete k, Coercible a (Identity a)) => a <~> Identity a
_Identity = _Coerce

_Const :: forall (a :: k) b. (Concrete k, Coercible a (Const a b)) => a <~> Const a b
_Const = _Coerce

_Compose :: forall i j (f :: j -> k) (g :: i -> j) (a :: i). 
            (Concrete k, Coercible (f (g a)) ((f ∘ g) a)) => f (g a) <~> (f ∘ g) a
_Compose = _Coerce

_Fix :: forall (f :: k -> k). (Concrete k, Coercible (f (Fix f)) (Fix f)) => f (Fix f) <~> Fix f
_Fix = _Coerce

swap :: forall (a :: k) b. Concrete k => a × b ~> b × a
swap = snd &&& fst

swapCoproduct :: forall (a :: k) b. Concrete k => a + b ~> b + a
swapCoproduct = inr ||| inl

apply :: forall (a :: k) b. Concrete k => (b^a × a) ~> b
apply = from _Curry id

fstUnit :: (Exponential m, Cartesian m, Final m) => Iso m (Product m (Terminal m) a) a
fstUnit = Iso snd (unit &&& id)

_Exp :: (Exponential m, Cartesian m, Final m) => Iso m (Product m (Terminal m) a) a
_Exp :: forall (a :: k) b.  (Final m, Exponential m

(a `m` b) <-> Terminal m `m` (Exp m b a)
_Exp = Iso
  (\f -> to _Curry $ f . to lunit)
  (\f -> from _Curry f . from lunit)

type Select = (~>) Unit
lunit :: forall (a :: k). Concrete k => Unit × a <~> a
lunit = Iso snd (unit &&& id)

type Select = (~>) Unit

_Exp :: forall (a :: k) b. Concrete k => (a ~> b) <-> Select (b^a)
_Exp = Iso
  (\f -> to _Curry $ f . to lunit)
  (\f -> from _Curry f . from lunit)

productUniversality :: forall (a :: k) b c. Concrete k => Select (a^c × b^c) <-> Select ((a × b)^c)
productUniversality = Iso
  (\p -> to _Exp (from _Exp (fst . p) &&& from _Exp (snd . p)))
  (\(from _Exp -> p) -> (to _Exp $ fst . p) &&& (to _Exp $ snd . p))

coproductUniversality :: forall (a :: k) b c. Concrete k => Select (c^a × c^b) <-> Select (c^(a + b))
coproductUniversality = Iso
  (\p -> to _Exp (from _Exp (fst . p) ||| from _Exp (snd . p)))
  (\(from _Exp -> p) -> (to _Exp $ p . inl) &&& (to _Exp $ p . inr))

{-
instance Concrete * where
  type (~>) = (->)

  _Coerce = Iso coerce coerce

  type Identity = Shadowed.Identity

  type Const = Shadowed.Const

  data Unit * = Unit
  unit = const Unit

  data Void *
  absurd v = case v of {}

  type (×) = (,)
  fst = Shadowed.fst
  snd = Shadowed.snd

  type (+) = Either
  inl = Left
  inr = Right

  distributivity = uncurry $ \c -> (c,) +++ (c,)

  newtype (f ∘ g) a = Compose0 { getCompose0 :: f (g a) }

  newtype b ^ a = Exp { runExp :: a -> b }
  _Curry = Iso (fmap Exp . curry) (uncurry . fmap runExp)

  newtype Free c a = Free0 { runFree0 :: forall b. c b => (a -> b) -> b }
  call a = Free0 (\f -> f a)
  interpret f (Free0 g) = g f

  newtype Fix f = Fix0 { unFix0 :: f (Fix f) }

instance Concrete (j -> *) where

  _Coerce = Iso1 coerce coerce

  type Identity = Identity'

  type Const = Const'

  data Unit (j -> *) (t :: j) = Unit1  -- Proxy
  unit = Natural1 (const Unit1)

  data Void (j -> *) (t :: j)
  absurd = Natural1 (\v -> case v of {})

  type (×) = Product
  fst = Natural1 (fst . getProduct1)
  snd = Natural1 (snd . getProduct1)

  type (+) = Coproduct
  inl = Natural1 (Coproduct1 . inl)
  inr = Natural1 (Coproduct1 . inr)

  distributivity = Natural1 $ cast distributivity where
    cast :: ((c x, Either (a x) (b x)) -> Either (c x, a x) (c x, b x))
         -> Product c (Coproduct a b) x -> Coproduct (Product c a) (Product
c b) x
    cast = coerce
    -- cast f = Coproduct1 . (Product1 +++ Product1) . f . fmap
getCoproduct1 . getProduct1

  newtype (b^a) x = Exp1 { runExp1 :: a x -> b x }
  _Curry = Iso
    (\f -> Natural1 $ fmap Exp1 . curry $ runNatural1 f . Product1)
    (\g -> Natural1 $ uncurry (runExp1 . runNatural1 g) . getProduct1)

  newtype (f ∘ g) a x = Compose1 { getCompose1 :: f (g a) x }

  newtype Free c a x = Free1 { runFree1 :: forall b. c b => (forall t. a t
-> b t) -> b x }
  call = Natural1 $ \ax -> Free1 $ \f -> f ax
  interpret (Natural1 f) = Natural1 $ \(Free1 g) -> g f

  newtype Fix f x = Fix1 { unFix1 :: f (Fix f) x }

pattern Iso1 :: forall (a :: j -> *) b. (forall x. a x -> b x) -> (forall x. b x -> a x) -> (Iso Natural a b)
pattern Iso1 { to1, from1 } = Iso (Natural1 to1) (Natural1 from1)

instance Category (Natural :: (j -> *) -> (j -> *) -> *) where
  id = Natural1 id
  Natural1 f . Natural1 g = Natural1 (f . g)

product1 :: (forall x. (a x, b x) -> (a' x, b' x)) -> Natural (a × b) (a' ×
b')
product1 f = Natural1 $ Product1 . f . getProduct1

sum1 :: (forall x. Either (a x) (b x) -> Either (a' x) (b' x)) -> Natural
(a + b) (a' + b')
sum1 f = Natural1 $ Coproduct1 . f . getCoproduct1

instance A (Natural :: (j -> *) -> (j -> *) -> *) where
  first (Natural1 f) = product1 (first f)
  second (Natural1 f) = product1 (second f)
  Natural1 f *** Natural1 g = product1 (f *** g)

  left (Natural1 f) = sum1 (left f)
  right (Natural1 f) = sum1 (right f)
  Natural1 f +++ Natural1 g = sum1 (f +++ g)

instance B (Natural :: (j -> *) -> (j -> *) -> *) where
  Natural1 f &&& Natural1 g = Natural1 $ Product1 . (f &&& g)
  Natural1 f ||| Natural1 g = Natural1 $ (f ||| g) . getCoproduct1


data family Product :: forall j. (j -> k) -> (j -> k) -> (j -> k)
newtype instance Product a b x = Product1 { getProduct1 :: (a x, b x) }
newtype instance Product a b x y = Product2 { getProduct2 :: (a x y, b x y)
}
newtype instance Product a b x y z = Product3 { getProduct3 :: (a x y z, b
x y z) }

data family Coproduct :: forall j. (j -> k) -> (j -> k) -> (j -> k)
newtype instance Coproduct a b x = Coproduct1 { getCoproduct1 :: a x + b x }
newtype instance Coproduct a b x y = Coproduct2 { getCoproduct2 :: a x y +
b x y }
newtype instance Coproduct a b x y z = Coproduct3 { getCoproduct3 :: a x y
z + b x y z }

data family Identity' :: forall j. (j -> k) -> (j -> k)
newtype instance Identity' a x = Identity'1 { runIdentity'1 :: a x }

data family Const' :: forall j i. (j -> k) -> i -> (j -> k)
newtype instance Const' a b x = Const'1 { getConst'1 :: a x }
-}
