{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Type.Coercion.Test where
import Data.Coerce
import Data.Type.Coercion
import Control.Category
import Prelude hiding ((.), id)
import Data.Monoid
import Test.Hspec
import Test.QuickCheck

eliminateSpec :: Spec
eliminateSpec = describe "eliminate" $ do
  describe "sumCoercesToProduct" $ do
    it "creates valid evidence for coerceWith . introduce" $ 
      property @(Int -> Bool) $ \n ->
        Product n == coerceWith (introduce sumCoercesToProduct) (Sum n)

    it "creates valid evidence for coerce" $
      property @(Int -> Bool) $ \n ->
        case sumCoercesToProduct of Coercion -> Sum n == coerce (Product n)

  describe "coerciblePair_1" $ do
    it "creates valid evidence for coerceWith . introduce" $ 
      property @(Int -> Int -> Bool) $ \m n -> gcoerceWith sumCoercesToProduct $
        Pair_1 (Sum m, Product n) == 
          coerceWith (introduce coerciblePair_1) (Pair_1 (Product m, Sum n))

    it "creates valid evidence for coerce" $
      property @(Int -> Int -> Bool) $ \m n -> gcoerceWith sumCoercesToProduct $
        case coerciblePair_1 @(*) @Sum @Product @Product @Sum of 
          Coercion -> Pair_1 (Product m, Sum n) == coerce (Pair_1 (Sum m, Product n))


sumCoercesToProduct :: Sum ~=~ Product
sumCoercesToProduct = eliminate Coercion

type family Pair = (p :: k -> k -> k) | p -> k where
  Pair = (,)
  Pair = Pair_

data family Pair_ :: (j -> k) -> (j -> k) -> j -> k
newtype instance Pair_ a b x = Pair_1 { getPair_1 :: Pair (a x) (b x) }

instance (Eq (a x), Eq (b x)) => Eq (Pair_ a b x) where
  Pair_1 (ax,bx) == Pair_1 (ax',bx') = ax == ax' && bx == bx'

coerciblePair_0 :: forall (a :: *) b a' b'. (Coercible a a', Coercible b b') => (Pair a b ~=~ Pair a' b')
coerciblePair_0 = Coercion

wrapPair_0 :: forall (a :: j -> *) b x. Pair (a x) (b x) ~=~ Pair a b x
wrapPair_0 = Coercion

coerciblePair_1 :: forall (a :: k -> *) b a' b'. (Coercible a a', Coercible b b') => (Pair a b ~=~ Pair a' b')
coerciblePair_1 = eliminate (wrapPair_0 . coerciblePair_0 . sym wrapPair_0)
