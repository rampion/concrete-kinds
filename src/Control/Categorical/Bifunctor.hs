{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- prevent GHC from complaining about Bifunctor
-- {-# OPTIONS_GHC -freduction-depth=0 #-}
module Control.Categorical.Bifunctor where

-- import qualified Prelude as Shadowed
-- import qualified Data.Bifunctor as Shadowed

import Prelude hiding (id, (.), Functor(..))
import Control.Category

import Control.Categorical.Functor
import Data.Dual
import Data.Type.Coercion
-- import Data.Universal

class ( forall x. Functor firstarrow arrow (bifunctor `Flipped` x)
      , forall x. Functor secondarrow arrow (bifunctor x)
      , Flippable bifunctor
      , Category arrow
      ) => Bifunctor (firstarrow :: firstobject -> firstobject -> *)
                     (secondarrow :: secondobject -> secondobject -> *)
                     (arrow :: object -> object -> *)
                     (bifunctor :: firstobject -> secondobject -> object) where

  first :: a `firstarrow` b -> bifunctor a x `arrow` bifunctor b x
  {-
  default first :: (Birepresentational arrow, Functor firstarrow arrow (bifunctor `Flipped` x))
                 => a `firstarrow` b -> bifunctor a x `arrow` bifunctor b x
  first = (case proof of Coercion -> pmap') where
    proof :: Flipped (Flipped bifunctor) `Coercion` bifunctor
    proof = involutionFlipped
    -}

  second :: c `secondarrow` d -> bifunctor x c `arrow` bifunctor x d
  default second :: Birepresentational arrow
                 => c `secondarrow` d -> bifunctor x c `arrow` bifunctor x d
  second = fmap-- qmap

  bimap :: a `firstarrow` b -> c `secondarrow` d -> bifunctor a c `arrow` bifunctor b d
  bimap f g = first @_ @_ @_ @_ @secondarrow f
            . second @_ @_ @_ @firstarrow g
{-

instance
    ( Shadowed.Bifunctor f
    , forall x. Shadowed.Functor (f x)
    ) => Bifunctor (->) (->) (->) f where
  first = Shadowed.first
  second = Shadowed.second
  bimap = Shadowed.bimap
-}

-- instance Bifunctor firstarrow secondarrow arrow bifunctor =>
--          Bifunctor secondarrow firstarrow arrow (Dual bifunctor)

{-
instance ( Bifunctor (arrow :: object -> object -> *) arrow arrow bifunctor
         , HasDual object
         ) =>
         Bifunctor (Dual arrow) (Dual arrow) (Dual arrow) bifunctor
instance Bifunctor (Dual (->)) (Dual (->)) (Dual (->)) (,) where
  first = undefined
  second = undefined
  bimap = undefined
         -}
