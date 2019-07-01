{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Kind.Concrete.Unit where
import Data.Type.Coercion
import Kind.Concrete.Const

type family Unit = (u :: k) where
  Unit = ()
  Unit = Const Unit

newtypeUnit_ :: WrapConst k => Unit ~=~ (Unit x :: k)
newtypeUnit_ = newtypeConst_
