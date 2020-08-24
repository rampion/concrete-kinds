{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Categorical.Profunctor where

import Prelude hiding (id, (.), Functor(..))
import Control.Category

import Control.Categorical.Functor
import Data.Dual

-- XXX: switch to deriving via approach so users can choose to either
--  - inherit form {Pea,Kew}functor
--  - inherit from Shadowed.Bifunctor
--  - define a custom dimap

class 
    ( Peafunctor profunctor (Flipped leftarrow) arrow
    , Kewfunctor profunctor rightarrow arrow
    , Flippable leftarrow
    , Category arrow
    ) => Profunctor (profunctor :: leftobject -> rightobject -> object)
                    (leftarrow :: leftobject -> leftobject -> *)
                    (rightarrow :: rightobject -> rightobject -> *)
                    (arrow :: object -> object -> *) where

  dimap :: a `leftarrow` b -> c `rightarrow` d -> profunctor b c `arrow` profunctor a d
  dimap f g = lmap @_ @_ @_ @_ @_ @rightarrow f
            . rmap @_ @_ @_ @_ @leftarrow @_ g

lmap :: Profunctor (profunctor :: leftobject -> rightobject -> object) leftarrow rightarrow arrow => a `leftarrow` b -> profunctor b x `arrow` profunctor a x
lmap = contrapmap

rmap :: Profunctor (profunctor :: leftobject -> rightobject -> object) leftarrow rightarrow arrow => c `rightarrow` d -> profunctor x c `arrow` profunctor x d
rmap = qmap

instance
    ( Peafunctor profunctor (Flipped leftarrow) arrow
    , Kewfunctor profunctor rightarrow arrow
    , Flippable leftarrow
    , Category arrow
    ) => Profunctor profunctor leftarrow rightarrow arrow

profunctor_example :: Int -> String
profunctor_example = dimap toEnum (replicate 5) succ

profunctor_example' :: Profunctor profunctor (->) (->) (->) => profunctor String Int -> profunctor Int String
profunctor_example' = dimap (replicate <*> toEnum) (replicate <*> toEnum)
