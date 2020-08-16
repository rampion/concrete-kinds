{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Categorical.Bifunctor where

import Prelude hiding (id, (.), Functor(..))
import Control.Category

import Control.Categorical.Functor
import Data.Dual

class 
    ( forall q. Functor (Flipped bifunctor q) firstarrow arrow
    , forall p. Functor (bifunctor p) secondarrow arrow
    , Flippable bifunctor
    , Category arrow
    , Birepresentational arrow
    ) => 
    Bifunctor (bifunctor :: firstobject -> secondobject -> object)
              (firstarrow :: firstobject -> firstobject -> *)
              (secondarrow :: secondobject -> secondobject -> *)
              (arrow :: object -> object -> *) where

  first :: a `firstarrow` b -> bifunctor a x `arrow` bifunctor b x
  -- this default signature shouldn't be necessary, since `Flipped` explicitly
  -- depends only on its first parameter and the Functor requirement for
  -- `Flipped bifunctor` was given above... but GHC-8.6.5 needs some convincing
  default first :: ( rotcnufib ~ Flipped bifunctor
                   , forall q. Functor (rotcnufib q) firstarrow arrow
                   )
                => a `firstarrow` b -> bifunctor a x `arrow` bifunctor b x
  first = pmap

  second :: c `secondarrow` d -> bifunctor x c `arrow` bifunctor x d
  second = qmap

  bimap :: a `firstarrow` b -> c `secondarrow` d -> bifunctor a c `arrow` bifunctor b d
  bimap f g = first @_ @_ @_ @_ @_ @secondarrow f
            . second @_ @_ @_ @_ @firstarrow g

bifunctor_example :: (Integer, String)
bifunctor_example = bimap (+1) reverse (0, "hello")

instance
    ( rotcnufib ~ Flipped bifunctor
    , forall q. Functor (rotcnufib q) firstarrow arrow
    , forall p. Functor (bifunctor p) secondarrow arrow
    , Flippable bifunctor
    , Birepresentational arrow
    , Category arrow
    ) => Bifunctor bifunctor firstarrow secondarrow arrow
