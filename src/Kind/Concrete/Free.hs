{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Kind.Concrete.Free where
import Data.Kind (Constraint)

data family Free :: (k -> Constraint) -> k -> k
newtype instance Free c a
  = Free { runFree :: forall b. c b => (a -> b) -> b }
newtype instance Free c a x
  = Free1 { runFree1 :: forall b. c b => (forall t. a t -> b t) -> b x }
newtype instance Free c a x y
  = Free2 { runFree2 :: forall b. c b => (forall t u. a t u -> b t u) -> b x y }
newtype instance Free c a x y z
  = Free3 { runFree3 :: forall b. c b => (forall t u v. a t u v -> b t u v) -> b x y z }

