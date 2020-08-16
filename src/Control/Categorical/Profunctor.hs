{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Categorical.Profunctor where

import Prelude hiding (id, (.), Functor(..))
import Control.Category

import Control.Categorical.Functor
import Data.Dual

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
