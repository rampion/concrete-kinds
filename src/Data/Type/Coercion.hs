{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
module Data.Type.Coercion
  ( module Shadowed
  , type (~=~)
  , introduce
  , eliminate
  ) where
import "base" Data.Type.Coercion as Shadowed
import Unsafe.Coerce

infix 0 ~=~
type (~=~) = Coercion

introduce :: (a ~=~ b) -> (forall x. a x ~=~ b x)
introduce Coercion = Coercion

-- TODO this is evil, better make sure this works
eliminate :: (forall x. a x ~=~ b x) -> (a ~=~ b)
eliminate = unsafeCoerce
