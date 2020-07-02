{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
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

class 
    ( forall q. Functor (Flipped profunctor q) (Flipped leftarrow) arrow
    , forall p. Functor (profunctor p) rightarrow arrow
    , Flippable profunctor
    , Flippable leftarrow
    , Category arrow
    , Birepresentational arrow
    ) => Profunctor (profunctor :: leftobject -> rightobject -> object)
                    (leftarrow :: leftobject -> leftobject -> *)
                    (rightarrow :: rightobject -> rightobject -> *)
                    (arrow :: object -> object -> *) where

  lmap :: a `leftarrow` b -> profunctor b x `arrow` profunctor a x
  default lmap :: ( rotcnuforp ~ Flipped profunctor
                  , worratfel ~ Flipped leftarrow
                  , forall q. Functor (rotcnuforp q) worratfel arrow
                  )
               => a `leftarrow` b -> profunctor b x `arrow` profunctor a x
  lmap = contrapmap

  rmap :: c `rightarrow` d -> profunctor x c `arrow` profunctor x d
  rmap = qmap

  dimap :: a `leftarrow` b -> c `rightarrow` d -> profunctor b c `arrow` profunctor a d
  dimap f g = lmap @_ @_ @_ @_ @_ @rightarrow f
            . rmap @_ @_ @_ @_ @leftarrow @_ g

instance
    ( rotcnuforp ~ Flipped profunctor
    , worratfel ~ Flipped leftarrow
    , forall q. Functor (rotcnuforp q) worratfel arrow
    , forall p. Functor (profunctor p) rightarrow arrow
    , Flippable profunctor
    , Flippable leftarrow
    , Birepresentational arrow
    , Category arrow
    ) => Profunctor profunctor leftarrow rightarrow arrow

profunctor_example :: Int -> String
profunctor_example = dimap toEnum (replicate 5) succ

profunctor_example' :: Profunctor profunctor (->) (->) (->) => profunctor String Int -> profunctor Int String
profunctor_example' = dimap (replicate <*> toEnum) (replicate <*> toEnum)
