{-# LANGUAGE PolyKinds #-}
module Control.Category.Coercible where

import qualified Data.Coerce as Shadowed

class Coercible (m :: k -> k -> *) where
  coerce :: Shadowed.Coercible a b => m a b

instance Coercible (->) where
  coerce = Shadowed.coerce
