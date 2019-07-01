{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Kind.Concrete.Natural where
import Prelude hiding ((.), id, fst, snd)
import Control.Category
import Control.Category.Strong
import Control.Category.Choice
import Control.Category.Cartesian
import Control.Category.Cocartesian
import Control.Category.Distributive
import Control.Category.Closed
import Control.Category.Final
import Control.Category.Initial
import Data.Iso
import Data.Type.Coercion
import Data.Coerce
import Kind.Concrete.Product
import Kind.Concrete.Sum
import Kind.Concrete.Exp
import Kind.Concrete.Unit
import Kind.Concrete.Void

type family (~>) = (m :: k -> k -> *) | m -> k where
  (~>) = (->)
  (~>) = Natural_
infixr 0 ~>

type (<~>) = Iso (~>)
infix 0 <~>

newtype Natural_ (a :: j -> k) (b :: j -> k) 
      = Natural_ { runNatural_ :: forall (x :: j). a x ~> b x }

--------------------------------------------------------------------------------
-- Use the corecursive structure of natural to define instances

class CanCoerce k where
  naturalCoerce :: forall (a :: k) (b :: k). Coercible a b => a ~> b

instance CanCoerce * where
  naturalCoerce = coerce

instance CanCoerce k => CanCoerce (j -> k) where
  naturalCoerce = Natural_ naturalCoerce

naturalCoerceBy :: forall (a :: k) (b :: k). CanCoerce k => a ~=~ b -> a ~> b
naturalCoerceBy Coercion = naturalCoerce

toNaturalIso :: forall (a :: k) b. CanCoerce k => (a ~=~ b) -> (a <~> b)
toNaturalIso p = Iso (naturalCoerceBy p) (naturalCoerceBy $ sym p)

_Coerce :: forall (a :: k) b. (CanCoerce k, Coercible a b) => a <~> b
_Coerce = toNaturalIso Coercion

_Product :: forall (a :: j -> k) b x. (CanCoerce k, WrapProduct k)
         => (a x × b x) <~> (a × b) x
_Product = toNaturalIso newtypeProduct_ 
         
_Sum :: forall (a :: j -> k) b x. (CanCoerce k, WrapSum k)
         => (a x + b x) <~> (a + b) x
_Sum = toNaturalIso newtypeSum_ 

_Unit :: (CanCoerce k, WrapUnit k) => Unit <~> (Unit x :: k)
_Unit = toNaturalIso newtypeUnit_

_Void :: (CanCoerce k, WrapVoid k) => Void <~> (Void x :: k)
_Void = toNaturalIso newtypeVoid_
         
_Exp :: forall (a :: j -> k) b x. (CanCoerce k, WrapExp k)
     => (a x ^ b x) <~> (a ^ b) x
_Exp = toNaturalIso newtypeExp_

wrap :: forall (a :: j -> k) b c d. 
        (forall x. (a x ~> b x) <-> (c x ~> d x)) -> ((a ~> b) <-> (c ~> d))
wrap i = Iso
  (\f -> Natural_ $   to i $ runNatural_ f)
  (\f -> Natural_ $ from i $ runNatural_ f)

{-
lift :: (forall x. a x <~> b x) -> (a <~> b)
lift i = Iso (Natural_ $ to i) (Natural_ $ from i)

lower :: (a <~> b) -> (forall x. a x <~> b x)
lower i = Iso (runNatural_ $ to i) (runNatural_ $ from i)

-- damn you impredictive types!
_Natural_ :: (forall x. a x <~> b x) <-> (a <~> b)
_Natural_ = Iso lift lower
-}

instance Category ((~>) ::           k  ->       k  -> *) =>
         Category (Natural_ :: (j -> k) -> (j -> k) -> *) where
  id = Natural_ id
  Natural_ f . Natural_ g = Natural_ (f . g)

instance ( Strong ((~>) :: k -> k -> *)
         , Product ((~>) :: k -> k -> *) ~ (×)
         , WrapProduct k
         , CanCoerce k
         ) => Strong (Natural_ :: (j -> k) -> (j -> k) -> *) where
  type Product Natural_ = (×)
  Natural_ f *** Natural_ g = Natural_ $ to _Product . (f *** g) . from _Product

instance ( Choice ((~>) :: k -> k -> *)
         , Coproduct ((~>) :: k -> k -> *) ~ (+)
         , WrapSum k
         , CanCoerce k
         ) => Choice (Natural_ :: (j -> k) -> (j -> k) -> *) where
  type Coproduct Natural_ = (+)
  Natural_ f +++ Natural_ g = Natural_ $ to _Sum . (f +++ g) . from _Sum

instance ( Cartesian ((~>) :: k -> k -> *)
         , Product ((~>) :: k -> k -> *) ~ (×)
         , WrapProduct k
         , CanCoerce k
         ) => Cartesian (Natural_ :: (j -> k) -> (j -> k) -> *) where
  fst = Natural_ $ fst . from _Product
  snd = Natural_ $ snd . from _Product
  Natural_ f &&& Natural_ g = Natural_ $ to _Product . (f &&& g)

instance ( Cocartesian ((~>) :: k -> k -> *)
         , Coproduct ((~>) :: k -> k -> *) ~ (+)
         , WrapSum k
         , CanCoerce k
         ) => Cocartesian (Natural_ :: (j -> k) -> (j -> k) -> *) where
  inl = Natural_ $ to _Sum . inl
  inr = Natural_ $ to _Sum . inr
  Natural_ f ||| Natural_ g = Natural_ $ (f ||| g) . from _Sum

instance ( Distributive ((~>) :: k -> k -> *)
         , Product ((~>) :: k -> k -> *) ~ (×)
         , Coproduct ((~>) :: k -> k -> *) ~ (+)
         , WrapProduct k
         , WrapSum k
         , CanCoerce k
         ) => Distributive (Natural_ :: (j -> k) -> (j -> k) -> *) where
  distribute = Natural_ 
             $ naturalCoerceBy (newtypeSum_ . (newtypeProduct_ ~+++~ newtypeProduct_))
             . distribute
             . naturalCoerceBy (sym $ newtypeProduct_ . (Coercion ~***~ newtypeSum_))

instance ( Final ((~>) :: k -> k -> *)
         , Terminal ((~>) :: k -> k -> *) ~ Unit
         , CanCoerce k
         , WrapUnit k
         ) => Final (Natural_ :: (j -> k) -> (j -> k) -> *) where
  type Terminal Natural_ = Unit_
  unit = Natural_ $ to _Unit . unit

instance ( Initial ((~>) :: k -> k -> *)
         , Coterminal ((~>) :: k -> k -> *) ~ Void
         , CanCoerce k
         , WrapVoid k
         ) => Initial (Natural_ :: (j -> k) -> (j -> k) -> *) where
  type Coterminal Natural_ = Void_
  absurd = Natural_ $ absurd . from _Void

instance ( Closed ((~>) :: k -> k -> *)
         , Product ((~>) :: k -> k -> *) ~ (×)
         , Exp ((~>) :: k -> k -> *) ~ (^)
         , WrapProduct k
         , WrapExp k
         , CanCoerce k
         ) => Closed (Natural_ :: (j -> k) -> (j -> k) -> *) where
  type Exp Natural_ = (^)
  _Closure = wrap $ precompose _Exp . _Closure . postcompose _Product

--------------------------------------------------------------------------------
-- Provide patterns for ease of use (`Natural3 f` vs `Natural_ (Natural_ (Natural_ f))`)

-- for completeness
pattern Natural0 :: (a -> b) -> (a ~> b)
pattern Natural0 { runNatural0 } = runNatural0
{-# COMPLETE Natural0 #-}

-- could be more simply defined as 
--
--    pattern Natural1 { runNatural1 } = Natural_ runNatural1
--
-- but it's instructive to see it as an example of how the the more complicated
-- patterns can be made
pattern Natural1 :: (forall x. a x -> b x) -> (a ~> b)
pattern Natural1 { runNatural1 } <- ((
    runNatural0 . runNatural_ :: (a ~> b) -> (forall x. a x -> b x)
  ) -> runNatural1)
  where Natural1 f = Natural_ (Natural0 f)
{-# COMPLETE Natural1 #-}

pattern Natural2 :: (forall x y. a x y -> b x y) -> (a ~> b)
pattern Natural2 { runNatural2 } <- ((
    runNatural1 . runNatural_ :: (a ~> b) -> (forall x y. a x y -> b x y)
  ) -> runNatural2) 
  where Natural2 f = Natural_ (Natural1 f)
{-# COMPLETE Natural2 #-}

pattern Natural3 :: (forall x y z. a x y z -> b x y z) -> (a ~> b)
pattern Natural3 { runNatural3 } <- ((
    runNatural2 . runNatural_ :: (a ~> b) -> (forall x y z. a x y z -> b x y z)
  ) -> runNatural3) 
  where Natural3 f = Natural_ (Natural2 f)
{-# COMPLETE Natural3 #-}
