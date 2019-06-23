{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Initial where
import Control.Category
import Data.Void

class Category m => Initial (m :: k -> k -> *) where
  type Coterminal m :: k
  absurd :: Coterminal m `m` a

instance Initial (->) where
  type Coterminal (->) = Void
  absurd = Data.Void.absurd
