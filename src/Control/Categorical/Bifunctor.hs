{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Control.Categorical.Bifunctor where

import Prelude hiding (id, (.), Functor(..))
import Control.Category

import Control.Categorical.Functor

class 
    ( Peafunctor bifunctor firstarrow arrow
    , Kewfunctor bifunctor secondarrow arrow
    , Category arrow
    ) => 
    Bifunctor (bifunctor :: firstobject -> secondobject -> object)
              (firstarrow :: firstobject -> firstobject -> *)
              (secondarrow :: secondobject -> secondobject -> *)
              (arrow :: object -> object -> *) where

  bimap :: a `firstarrow` b -> c `secondarrow` d -> bifunctor a c `arrow` bifunctor b d
  bimap f g = first @_ @_ @_ @_ @_ @secondarrow f
            . second @_ @_ @_ @_ @firstarrow g

first :: Bifunctor (bifunctor :: firstobject -> secondobject -> object) firstarrow secondarrow arrow => a `firstarrow` b -> bifunctor a x `arrow` bifunctor b x
first = pmap

second :: Bifunctor (bifunctor :: firstobject -> secondobject -> object) firstarrow secondarrow arrow => c `secondarrow` d -> bifunctor x c `arrow` bifunctor x d
second = qmap

instance
    ( Peafunctor bifunctor firstarrow arrow
    , Kewfunctor bifunctor secondarrow arrow
    , Category arrow
    ) => Bifunctor bifunctor firstarrow secondarrow arrow

bifunctor_example :: (Integer, String)
bifunctor_example = bimap (+1) reverse (0, "hello")
