{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Void where
import qualified Data.Void as Shadowed
import Data.Type.Coercion
import Kind.Concrete.Const

type family Void = (v :: k) where
  Void = Shadowed.Void
  Void = Const Void

newtypeVoid_ :: WrapConst k => Void ~=~ (Void x :: k)
newtypeVoid_ = newtypeConst_
