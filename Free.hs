{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- for instances
{-# LANGUAGE AllowAmbiguousTypes #-}
-- for examples
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Free where
import GHC.Types
-- for instances
import Data.Bifunctor

type (a :: k) ~> (b :: k) = Morphism k a b

newtype Natural (f :: j -> k) (g :: j -> k) = 
  Natural { getNatural :: forall (x :: j). f x ~> g x }

type family Morphism k :: k -> k -> Type where
  Morphism Type = (->)
  Morphism (j -> k) = Natural

class DataKind k where
  data Free :: (k -> Constraint) -> k -> k
  interpret :: forall (cls :: k -> Constraint) (u :: k) (v :: k). 
               cls v => (u ~> v) -> (Free cls u ~> v)
  call :: forall (cls :: k -> Constraint) (u :: k). 
          u ~> Free cls u

instance DataKind Type where
  newtype Free cls u = Free0
    { runFree0 :: forall v. cls v => (u ~> v) -> v }
  interpret f = \(Free0 g) -> g f
  call = \u -> Free0 $ \f -> f u

instance DataKind (k -> Type) where
  newtype Free cls u a = Free1
    { runFree1 :: forall v. cls v => (u ~> v) -> v a }
  interpret f = Natural $ \(Free1 g) -> g f
  call = Natural $ \ux -> Free1 $ \f -> getNatural f ux

instance DataKind (j -> k -> Type) where
  newtype Free cls u a b = Free2
    { runFree2 :: forall v. cls v => (u ~> v) -> v a b }
  interpret f = Natural $ Natural $ \(Free2 g) -> g f
  call = Natural $ Natural $ \uxy -> Free2 $ \f -> getNatural f `getNatural` uxy

-- https://stackoverflow.com/questions/56573436
class (a => b) => a ⇒ b
instance (a => b) => a ⇒ b

type f ⊆ g = (forall (x :: k). f x ⇒ g x)

{-
instance cls ⊆ Show => Show (Free cls u) where
  showsPrec n (Free0 g) = _ $ g id
-}

-- this is pseudo-applicative
op0 :: forall kls cls u. cls ⊆ kls => (forall v. kls v => v -> v -> v) -> Free cls u -> Free cls u -> Free cls u
op0 (#) (Free0 g) (Free0 g') = Free0 $ \f -> g f # g' f

-- pseudo-functor
lift0 :: forall kls cls u. cls ⊆ kls => (forall v. kls v => v -> v) -> Free cls u -> Free cls u
lift0 h (Free0 g) = Free0 $ h . g

instance cls ⊆ Num => Num (Free cls u) where
  (+) = op0 @Num (+)
  (*) = op0 @Num (*)
  (-) = op0 @Num (-)
  abs = lift0 @Num abs
  signum = lift0 @Num signum

instance cls ⊆ Semigroup => Semigroup (Free cls u) where
  (<>) = op0 @Semigroup (<>)

instance cls ⊆ Monoid => Monoid (Free cls u) where
  mempty = Free0 $ \_ -> mempty
  mappend = (<>)

semigroupExample :: forall (cls :: Type -> Constraint) (u :: Type).
                    (Num u, cls ⊆ Semigroup) => Free cls u
semigroupExample = call 1 <> call 2 <> call 3

-- |
-- >>> v0
-- [1,2,3]
v0 :: Num a => [a]
v0 = interpret (pure @[]) (semigroupExample @Monoid) -- (\x -> [x]) (semigroupExample @Monoid)

instance Functor (Free cls) where
  fmap f (Free0 g) = Free0 $ \h -> g (h . f)

instance (cls ⊆ Bifunctor) => Functor (Free cls u x) where
  -- or cls ⊆ Profunctor, or cls ⊆ Arrow, or...
  -- need either Functor1 or a way to structurally match typeclasses
  fmap f (Free2 g) = Free2 $ second f . g

-- this is probably better
instance (cls ⊆ Bifunctor) => Bifunctor (Free cls u) where
  bimap f f' (Free2 g) = Free2 $ bimap f f' . g
  first f (Free2 g) = Free2 $ first f . g
  second f (Free2 g) = Free2 $ second f . g

instance cls ⊆ Functor => Functor (Free cls u) where
  fmap f (Free1 g) = Free1 $ fmap f . g

instance cls ⊆ Applicative => Applicative (Free cls u) where
  pure a = Free1 $ \_ -> pure a
  Free1 g <*> Free1 h = Free1 $ \f -> g f <*> h f

instance cls ⊆ Monad => Monad (Free cls u) where
  Free1 g >>= h = Free1 $ \f -> g f >>= flip runFree1 f . h

data Inst a where
  Op :: Inst (Int -> Int)
  Val :: Inst Int

applicativeExample :: forall (cls :: (Type -> Type) -> Constraint).
                      cls ⊆ Applicative => Free cls Inst Int
applicativeExample = getNatural call Op <*> getNatural call Val

-- |
-- >>> v1
-- [1,5,2,6,10,50]
v1 :: [Int]
v1 = interpret (Natural go) `getNatural` (applicativeExample @Monad) where
  go :: Inst x -> [x] 
  go Op = [ id, succ, (10*) ]
  go Val = [ 1, 5 ]
