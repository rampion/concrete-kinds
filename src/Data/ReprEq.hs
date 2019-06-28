{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
module Data.ReprEq where
import Data.Coerce
import Control.Category
import Unsafe.Coerce

-- TODO : replace with Data.Type.Coercion

data a ~=~ b where
  IsCoercible :: Coercible a b => a ~=~ b
infix 0 ~=~

sym :: (a ~=~ b) -> (b ~=~ a)
sym IsCoercible = IsCoercible

instance Category (~=~) where
  id = IsCoercible
  IsCoercible . IsCoercible = IsCoercible

coerceBy :: a ~=~ b -> a -> b
coerceBy IsCoercible = coerce

coerceWith :: a ~=~ b -> (Coercible a b => r) -> r
coerceWith IsCoercible r = r

introduce :: (a ~=~ b) -> (forall x. a x ~=~ b x)
introduce IsCoercible = IsCoercible

-- TODO this is evil, better make sure this works
eliminate :: (forall x. a x ~=~ b x) -> (a ~=~ b)
eliminate = unsafeCoerce
