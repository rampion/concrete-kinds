{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Proto where
import Prelude hiding (id, (.), curry, uncurry)
import Control.Category

import Data.Coerce

import qualified Prelude                as Shadowed
import qualified Data.Void              as Shadowed
import qualified Data.Functor.Const     as Shadowed
import qualified Data.Functor.Compose   as Shadowed
import qualified Data.Functor.Identity  as Shadowed

{-
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
-}

data Iso arrow a b = Iso 
  { forwards :: arrow a b
  , backwards :: arrow b a
  }

instance Category arrow => Category (Iso arrow) where
  id = Iso id id
  Iso f f' . Iso g g' = Iso (f . g) (g' . f')

-- ~~> natural
-- --> exponential
--
-- (Unit --> a) <~~> a
-- ((a --> b) -*- a) ~~> b
--
-- id :: a ~~> a

apply :: forall k (a :: k) (b :: k). Concrete k => ((a --> b) -*- a) ~~> b
apply = backwards _Curry id

initial :: forall k (a :: k). Concrete k => Void ~~> a
initial = undefined

final :: forall k (a :: k). Concrete k => a ~~> Unit
final = undefined

exponential :: forall k (a :: k) (b :: k). (a ~~> b) <-> (Unit ~~> (a --> b))
exponential = undefined

type (<->) = Iso (->)
type (<~~>) = Iso (~~>)

curry :: forall k (a :: k) b c. Concrete k => (a -*- b ~~> c) -> (a ~~> b --> c)
curry = forwards _Curry

uncurry :: forall k (a :: k) b c. Concrete k =>  (a ~~> b --> c) -> (a -*- b ~~> c)
uncurry = backwards _Curry

rev :: (a <-> b) -> (b <-> a)
rev (Iso f g) = Iso g f

class ( forall (a :: k). Coercible a (Identity a)
      , forall j (a :: k) (b :: j). Coercible a (Const a b)
      , forall i j (f :: j -> k) (g :: i -> j) (a :: i). Coercible (f (g a)) (Compose f g a)
      , Category ((~~>) :: k -> k -> *)
      ) => Concrete k where

  _Curry      :: forall (a :: k) b c. (a -*- b ~~> c) <-> (a ~~> b --> c)
  _Arrow      :: forall j (a :: j -> k) (b :: j -> k) (x :: j). (a x --> b x) <~~> (a --> b) x
  _Product    :: forall j (a :: j -> k) (b :: j -> k) (x :: j). (a x -*- b x) <~~> (a -*- b) x
  _Coproduct  :: forall j (a :: j -> k) (b :: j -> k) (x :: j). (a x -+- b x) <~~> (a -+- b) x

instance Concrete * where
  _Curry      = Iso Shadowed.curry Shadowed.uncurry
  _Arrow      = _Lift1
  _Product    = _Lift1
  _Coproduct  = _Lift1

_Coerce :: Coercible a b => a <-> b
_Coerce = Iso coerce coerce

_Lift1 :: op (a x) (b x) <-> Binary op a b x
_Lift1 = _Coerce -- Iso Binary1 getBinary1

{-
(Concrete j, Concrete k) => forall (f :: j -> k). 
  (f a --> f b) <~~> f (a --> b)
(Concrete j, Concrete k) => forall (f :: j -> k). 
  (f a -*- f b) <~~> f (a -*- b)
(Concrete j, Concrete k) => forall (f :: j -> k). 
  (f a -+- f b) <~~> f (a -+- b)
-}

_Lift2 :: Iso (Universal (->)) (Binary op (a x) (b x)) (Binary op a b x)
_Lift2 = Iso 
    { forwards  = Universal $ Binary2 . getBinary1
    , backwards = Universal $ Binary1 . getBinary2
    }

instance Concrete (k -> *) where
  _Curry        = Iso
    { forwards  = \(Universal f) -> Universal $ \ax -> Binary1 $ \bx -> f $ Binary1 (ax, bx)
    , backwards = \(Universal f) -> Universal $ \(Binary1 (ax, bx)) -> f ax `getBinary1` bx
    }
  _Arrow        = _Lift2
  _Product      = _Lift2
  _Coproduct    = _Lift2

{-
instance Concrete (j -> k -> *) where
  curry (Universal f) = Universal $ Universal $ \axy -> Binary2 $ \bxy -> getUniversal f $ Binary2 (axy, bxy)

instance Concrete (i -> j -> k -> *) where
  curry (Universal f) = Universal $ Universal $ Universal $ \axyz ->
    Binary3 $ \bxyz -> getUniversal (getUniversal f) $ Binary3 $ (axyz, bxyz)
    -}

type family Unit = (unit :: k) | unit -> k where
  Unit = ()
  Unit = Nonary ()

type family Void = (void :: k) | void -> k where
  Void = Shadowed.Void
  Void = Nonary Shadowed.Void

type family Identity = (identity :: k -> k) | identity -> k where
  Identity = Shadowed.Identity
  Identity = Identity_

data family Identity_ (a :: j -> k) :: j -> k
newtype instance Identity_ a x = Identity1 { getIdentity1 :: a x }
newtype instance Identity_ a x y = Identity2 { getIdentity2 :: a x y }
newtype instance Identity_ a x y z = Identity3 { getIdentity3 :: a x y z }

type family Const = (const :: k -> j -> k) | const -> k where
  Const = Shadowed.Const
  Const = Const_

data family Const_ (a :: j -> k) (b :: i) :: j -> k
newtype instance Const_ a b x = Const1 { getConst1 :: a x }
newtype instance Const_ a b x y = Const2 { getConst2 :: a x y }
newtype instance Const_ a b x y z = Const3 { getConst3 :: a x y z }

type family Compose = (compose :: (j -> k) -> (i -> j) -> i -> k) | compose -> k where
  Compose = Shadowed.Compose
  Compose = Compose_

data family Compose_ (a :: i -> j -> k) (b :: h -> i) :: h -> j -> k
newtype instance Compose_ a b w x = Compose1 { getCompose1 :: a (b w) x }
newtype instance Compose_ a b w x y = Compose2 { getCompose2 :: a (b w) x y }
newtype instance Compose_ a b w x y z = Compose3 { getCompose3 :: a (b w) x y z }

data family Subst :: (i -> j -> k) -> (i -> j) -> i -> k
newtype instance Subst a b c = Subst { getSubst :: a c (b c) }
newtype instance Subst a b c x = Subst1 { getSubst1 :: a c (b c) x }
newtype instance Subst a b c x y = Subst2 { getSubst2 :: a c (b c) x y }
newtype instance Subst a b c x y z = Subst3 { getSubst3 :: a c (b c) x y z }

type family (-*-) = (product :: k -> k -> k) | product -> k where
  (-*-) = (,)
  (-*-) = Binary (,)
infixl 7 -*-

type family (-+-) = (coproduct :: k -> k -> k) | coproduct -> k where
  (-+-) = Either
  (-+-) = Binary Either
infixl 7 -+-

type family (-->) = (arrow :: k -> k -> k) | arrow -> k where
  (-->) = (->)
  (-->) = Binary (->)
infixr 0 -->

type family (~~>) = (arrow :: k -> k -> *) | arrow -> k where
  (~~>) = (->)
  (~~>) = Universal (~~>)
infixr 0 ~~>

data family Nonary (obj :: *) :: j -> k
newtype instance Nonary obj x = Nonary1 { getNonary1 :: obj }
newtype instance Nonary obj x y = Nonary2 { getNonary2 :: obj }
newtype instance Nonary obj x y z = Nonary3 { getNonary3 :: obj }

data family Binary (op :: * -> * -> *) (a :: j -> k) (b :: j -> k) :: j -> k
newtype instance Binary op a b x = Binary1 { getBinary1 :: a x `op` b x }
newtype instance Binary op a b x y = Binary2 { getBinary2 :: a x y `op` b x y }
newtype instance Binary op a b x y z = Binary3 { getBinary3 :: a x y z `op` b x y z }

newtype Universal (op :: k -> k -> *) (a :: j -> k) (b :: j -> k) 
  = Universal { getUniversal :: forall x. a x `op` b x }

instance Category arrow => Category (Universal arrow) where
  id = Universal id
  Universal p . Universal q = Universal (p . q)
