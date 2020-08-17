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
module Control.Categorical.Bifunctor_ where

import Prelude hiding (id, (.), Functor(..))
import Control.Category

import Control.Categorical.Functor
import Data.Dual

-- keep this copy around for now, we were having issues with it before
--
-- maybe MonoLocalBinds would help? It did in Associative.
--
-- TODO: check Functor, Profunctor for similar patterns

class 
    ( Peafunctor bifunctor firstarrow arrow
    , Kewfunctor bifunctor secondarrow arrow
    , Flippable bifunctor
    , Category arrow
    , Birepresentational arrow
    ) => 
    Bifunctor (bifunctor :: firstobject -> secondobject -> object)
              (firstarrow :: firstobject -> firstobject -> *)
              (secondarrow :: secondobject -> secondobject -> *)
              (arrow :: object -> object -> *) where

  first :: a `firstarrow` b -> bifunctor a x `arrow` bifunctor b x
  first = pmap

  second :: c `secondarrow` d -> bifunctor x c `arrow` bifunctor x d
  second = qmap

  bimap :: a `firstarrow` b -> c `secondarrow` d -> bifunctor a c `arrow` bifunctor b d
  bimap f g = first @_ @_ @_ @_ @_ @secondarrow f
            . second @_ @_ @_ @_ @firstarrow g
instance
    ( Peafunctor bifunctor firstarrow arrow
    , Kewfunctor bifunctor secondarrow arrow
    , Flippable bifunctor
    , Category arrow
    , Birepresentational arrow
    ) => Bifunctor bifunctor firstarrow secondarrow arrow

bifunctor_example :: (Integer, String)
bifunctor_example = bimap (+1) reverse (0, "hello")
