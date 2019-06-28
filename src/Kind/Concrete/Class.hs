{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Kind.Concrete.Class where
import Control.Category.Strong
import Control.Category.Cartesian
import Control.Category.Choice
import Control.Category.Cocartesian
import Control.Category.Distributive
import Control.Category.Closed
import Control.Category.Initial
import Control.Category.Final

import Kind.Concrete.Product
import Kind.Concrete.Sum
import Kind.Concrete.Exp
import Kind.Concrete.Unit
import Kind.Concrete.Void
import Kind.Concrete.Natural

class ( Distributive ((~>) :: k -> k -> *) 
      , Cartesian ((~>) :: k -> k -> *) 
      , Cocartesian ((~>) :: k -> k -> *) 
      , Closed ((~>) :: k -> k -> *) 
      , Initial ((~>) :: k -> k -> *) 
      , Final ((~>) :: k -> k -> *) 
      , Product ((~>) :: k -> k -> *) ~ (×)
      , Coproduct ((~>) :: k -> k -> *) ~ (+)
      , Exp ((~>) :: k -> k -> *) ~ (^)
      , Terminal ((~>) :: k -> k -> *) ~ Unit
      , Coterminal ((~>) :: k -> k -> *) ~ Void
      ) => Concrete k where

instance Concrete *
instance Concrete (k -> *)
instance Concrete (j -> k -> *)
instance Concrete (i -> j -> k -> *)
