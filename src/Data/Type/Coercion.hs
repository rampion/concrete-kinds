{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Data.Type.Coercion
  ( module Shadowed
  , type (~=~)
  ) where
import "base" Data.Type.Coercion as Shadowed

infix 0 ~=~
type (~=~) = Coercion
