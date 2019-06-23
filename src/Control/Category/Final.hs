{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Final where
import Control.Category

class Category m => Final (m :: k -> k -> *) where
  type Terminal m :: k
  unit :: a `m` Terminal m

instance Final (->) where
  type Terminal (->) = ()
  unit = const ()
