{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- prevent GHC from complaining about Profunctor
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Control.Categorical.Profunctor where

import Prelude hiding (id, (.), Functor(..))
import Control.Category

import Control.Categorical.Functor
import Data.Dual
import Data.Universal

class ( Functor (Dual leftarrow) (Universal arrow) profunctor 
      , Functor rightarrow (Universal arrow) (Dual profunctor)
      , Category arrow
      ) => Profunctor (leftarrow :: leftobject -> leftobject -> *)
                      (rightarrow :: rightobject -> rightobject -> *)
                      (arrow :: object -> object -> *)
                      (profunctor :: leftobject -> rightobject -> object) where
  lmap :: a `leftarrow` b -> profunctor b x `arrow` profunctor a x
  default lmap
    :: HasDual leftobject
    => a `leftarrow` b -> profunctor b x `arrow` profunctor a x
  lmap = contrapmap

  rmap :: c `rightarrow` d -> profunctor x c `arrow` profunctor x d
  default rmap
    :: ( HasDual object
       , Birepresentational arrow
       )
    => c `rightarrow` d -> profunctor x c `arrow` profunctor x d
  rmap = qmap

  dimap
    :: a `leftarrow` b
    -> c `rightarrow` d
    -> profunctor b c `arrow` profunctor a d
  dimap f g = lmap @_ @_ @_ @_ @rightarrow f
            . rmap @_ @_ @_ @leftarrow g

instance Profunctor (->) (->) (->) (->)
