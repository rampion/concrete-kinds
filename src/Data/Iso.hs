{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
module Data.Iso where
import Control.Category
import Control.Category.Strong
import Control.Category.Choice
import Data.Coerce
import Prelude hiding (id, (.), fst, snd)

data Iso (m :: k -> k -> *) a b = Iso { to :: m a b, from :: m b a }

type (<->) = Iso (->)
infix 0 <->

coerceIso :: (Coercible a a', Coercible b b') => (a <-> b) -> (a' <-> b')
-- can't use `coerceIso = coerce` because GHC infers
--
--  type role Iso representational nominal nominal
--
-- since it can't infer what roles `m`'s parameters will have, and there's no
-- way (yet) to require that m have representational parameters.
--
-- see https://stackoverflow.com/questions/56793753
coerceIso (Iso f f') = Iso (coerce f) (coerce f')

rev :: Iso m a b -> Iso m b a
rev (Iso f f') = Iso f' f

fmapIso :: Functor f => (a <-> b) -> (f a <-> f b)
fmapIso (Iso f f') = Iso (fmap f) (fmap f')

precompose :: Category m => Iso m b c -> (m a b <-> m a c)
precompose (Iso f f') = Iso (f.) (f'.)

postcompose :: Category m => Iso m a b -> (m b c <-> m a c)
postcompose (Iso f f') = Iso (.f) (.f')

_Curry :: ((a,b) -> c) <-> (a -> b -> c)
_Curry = Iso curry uncurry

instance Category m => Category (Iso m) where
  id = Iso id id
  Iso f f' . Iso g g' = Iso (f . g) (g' . f')

instance Strong m => Strong (Iso m) where
  type Product (Iso m) = Product m
  first (Iso f f') = Iso (first f) (first f')
  second (Iso f f') = Iso (second f) (second f')
  Iso f f' *** Iso g g' = Iso (f *** g) (f' *** g')

{-
_Product :: Cartesian m => (c `m` a, c `m` b) <-> (c `m` Product m a b)
_Product = Iso (uncurry (&&&)) ((fst.) &&& (snd.))
-}

instance Choice m => Choice (Iso m) where
  type Coproduct (Iso m) = Coproduct m
  left (Iso f f') = Iso (left f) (left f')
  right (Iso f f') = Iso (right f) (right f')
  Iso f f' +++ Iso g g' = Iso (f +++ g) (f' +++ g')

{-
_Sum :: Cocartesian m => (a `m` c, b `m` c) <-> (Coproduct m a b `m` c)
_Sum = Iso (uncurry (|||)) ((.inl) &&& (.inr))
-}
