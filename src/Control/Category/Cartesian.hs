{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Cartesian where
import Control.Category
import Control.Category.Strong
import qualified Control.Arrow as Shadowed
import qualified Prelude as Shadowed

class Strong m => Cartesian m where
  fst :: Product m a b `m` a
  snd :: Product m a b `m` b
  (&&&) :: c `m` a -> c `m` b -> c `m` Product m a b
  infixr 3 &&&

instance Cartesian (->) where
  fst = Shadowed.fst
  snd = Shadowed.snd
  (&&&) = (Shadowed.&&&)

swap :: Cartesian m => Product m a b `m` Product m b a
swap = snd &&& fst

diag :: Cartesian m => a `m` Product m a a
diag = id &&& id
