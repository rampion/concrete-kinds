{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Kind.Concrete.Free where
import Data.Kind (Constraint)
import Kind.Concrete.Natural
import Kind.Concrete.Class
import Control.Category
import Prelude hiding ((.),id)

class Concrete k => HasFree k where
  data Free :: (k -> Constraint) -> k -> k
  call :: forall (a :: k) (c :: k -> Constraint). a ~> Free c a
  interpret :: forall (a :: k) b (c :: k -> Constraint). c b => (a ~> b) -> Free c a ~> b

-- they're just really specialized Readers
  
instance HasFree * where
  newtype instance Free c a = Free0 { runFree0 :: forall b. c b => (a -> b) -> b }
  call = Natural0 $ \a -> Free0 ($a)
  interpret f = Natural0 $ (`runFree0` runNatural0 f)

instance HasFree (k -> *) where
  newtype instance Free c a x = Free1 { runFree1 :: forall b. c b => (forall t. a t -> b t) -> b x }
  call = Natural1 $ \a -> Free1 ($a)
  interpret f = Natural1 $ (`runFree1` runNatural1 f)

instance HasFree (j -> k -> *) where
  newtype Free c a x y = Free2 { runFree2 :: forall b. c b => (forall t u. a t u -> b t u) -> b x y }
  call = Natural2 $ \a -> Free2 ($a)
  interpret f = Natural2 $  (`runFree2` runNatural2 f)

instance HasFree (i -> j -> k -> *) where
  newtype Free c a x y z = Free3 { runFree3 :: forall b. c b => (forall t u v. a t u v -> b t u v) -> b x y z }
  call = Natural3 $ \a -> Free3 ($a)
  interpret f = Natural3 $  (`runFree3` runNatural3 f)

-- Use an intermediate class to delay putting the (a => b) instance in scope
-- See https://stackoverflow.com/questions/56573436
class (a => b) => a ⇒ b
instance (a => b) => a ⇒ b

type f ⊆ g = (forall (x :: k). f x ⇒ g x)

class (f & g) a
instance (f a, g a) => (f & g) a

-- Identity functor in disguise
instance Functor (Free cls) where
  fmap f (Free0 g) = Free0 $ \h -> g (h . f)

-- monoapplicative
op0 :: forall kls cls u. cls ⊆ kls => (forall v. kls v => v -> v -> v) -> Free cls u -> Free cls u -> Free cls u
op0 (#) (Free0 g) (Free0 g') = Free0 $ \f -> g f # g' f

-- monofunctor
lift0 :: forall kls cls u. cls ⊆ kls => (forall v. kls v => v -> v) -> Free cls u -> Free cls u
lift0 h (Free0 g) = Free0 $ h . g

instance cls ⊆ Num => Num (Free cls u) where
  (+) = op0 @Num (+)
  (*) = op0 @Num (*)
  (-) = op0 @Num (-)
  abs = lift0 @Num abs
  signum = lift0 @Num signum
  fromInteger i = Free0 $ \_ -> fromInteger i

instance cls ⊆ Semigroup => Semigroup (Free cls u) where
  (<>) = op0 @Semigroup (<>)

instance cls ⊆ Monoid => Monoid (Free cls u) where
  mempty = Free0 $ \_ -> mempty
  mappend = (<>)

instance cls ⊆ Functor => Functor (Free cls u) where
  fmap f (Free1 ma) = Free1 $ \g -> fmap f $ ma g

instance cls ⊆ Applicative => Applicative (Free cls u) where
  pure a = Free1 $ \_ -> pure a
  Free1 mf <*> Free1 ma = Free1 $ \g -> mf g <*> ma g

instance cls ⊆ Monad => Monad (Free cls u) where
  Free1 ma >>= f = Free1 $ \g -> ma g >>= (`runFree1` g) . f

instance cls ⊆ Category => Category (Free cls u) where
  id = Free2 $ \_ -> id
  Free2 f . Free2 g = Free2 $ \h -> f h . g h

----------

-- what doesn't it provide free instances of?
{-
instance cls ⊆ Show => Show (Free cls u) where
  showsPrec p (Free0 f) = _ p

instance cls ⊆ Read => Read (Free cls u) where
  readsPrec p s = _
-}
