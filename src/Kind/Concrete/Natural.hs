{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Kind.Concrete.Natural where
import Prelude hiding ((.), id)
import Control.Category
import Control.Category.Coercible
import Data.Iso

type family (~>) = (m :: k -> k -> *) | m -> k where
  (~>) = (->)
  (~>) = Natural_
infixr 0 ~>

type (<~>) = Iso (~>)
infix 0 <~>

newtype Natural_ (a :: j -> k) (b :: j -> k) 
      = Natural_ { runNatural_ :: forall (x :: j). a x ~> b x }

-- corecursive structure if you want it
instance Category (Natural_ :: (j -> *) -> (j -> *) -> *) where
  id = Natural_ id
  Natural_ f . Natural_ g = Natural_ (f . g)

instance Category (Natural_ ::      (j -> k) ->      (j -> k) -> *) =>
         Category (Natural_ :: (i -> j -> k) -> (i -> j -> k) -> *) where
  id = Natural_ id
  Natural_ f . Natural_ g = Natural_ (f . g)

instance Coercible (Natural_ :: (j -> *) -> (j -> *) -> *) where
  coerce = Natural_ coerce

instance Coercible (Natural_ ::      (j -> k) ->      (j -> k) -> *) =>
         Coercible (Natural_ :: (i -> j -> k) -> (i -> j -> k) -> *) where
  coerce = Natural_ coerce

-- for completeness
pattern Natural0 :: (a -> b) -> (a ~> b)
pattern Natural0 { runNatural0 } = runNatural0
{-# COMPLETE Natural0 #-}

-- could be more simply defined as 
--
--    pattern Natural1 { runNatural1 } = Natural_ runNatural1
--
-- but it's instructive to see it as an example of how the the more complicated
-- patterns can be made
pattern Natural1 :: (forall x. a x -> b x) -> (a ~> b)
pattern Natural1 { runNatural1 } <- ((
    runNatural0 . runNatural_ :: (a ~> b) -> (forall x. a x -> b x)
  ) -> runNatural1)
  where Natural1 f = Natural_ (Natural0 f)
{-# COMPLETE Natural1 #-}

pattern Natural2 :: (forall x y. a x y -> b x y) -> (a ~> b)
pattern Natural2 { runNatural2 } <- ((
    runNatural1 . runNatural_ :: (a ~> b) -> (forall x y. a x y -> b x y)
  ) -> runNatural2) 
  where Natural2 f = Natural_ (Natural1 f)
{-# COMPLETE Natural2 #-}

pattern Natural3 :: (forall x y z. a x y z -> b x y z) -> (a ~> b)
pattern Natural3 { runNatural3 } <- ((
    runNatural2 . runNatural_ :: (a ~> b) -> (forall x y z. a x y z -> b x y z)
  ) -> runNatural3) 
  where Natural3 f = Natural_ (Natural2 f)
{-# COMPLETE Natural3 #-}

pattern Natural4 :: (forall w x y z. a w x y z -> b w x y z) -> (a ~> b)
pattern Natural4 { runNatural4 } <- ((
    runNatural3 . runNatural_ :: (a ~> b) -> (forall w x y z. a w x y z -> b w x y z)
  ) -> runNatural4) 
  where Natural4 f = Natural_ (Natural3 f)
{-# COMPLETE Natural4 #-}

pattern Natural5 :: (forall v w x y z. a v w x y z -> b v w x y z) -> (a ~> b)
pattern Natural5 { runNatural5 } <- ((
    runNatural4 . runNatural_ :: (a ~> b) -> (forall v w x y z. a v w x y z -> b v w x y z)
  ) -> runNatural5) 
  where Natural5 f = Natural_ (Natural4 f)
{-# COMPLETE Natural5 #-}

pattern Natural6 :: (forall u v w x y z. a u v w x y z -> b u v w x y z) -> (a ~> b)
pattern Natural6 { runNatural6 } <- ((
    runNatural5 . runNatural_ :: (a ~> b) -> (forall u v w x y z. a u v w x y z -> b u v w x y z)
  ) -> runNatural6) 
  where Natural6 f = Natural_ (Natural5 f)
{-# COMPLETE Natural6 #-}

pattern Natural7 :: (forall t u v w x y z. a t u v w x y z -> b t u v w x y z) -> (a ~> b)
pattern Natural7 { runNatural7 } <- ((
    runNatural6 . runNatural_ :: (a ~> b) -> (forall t u v w x y z. a t u v w x y z -> b t u v w x y z)
  ) -> runNatural7) 
  where Natural7 f = Natural_ (Natural6 f)
{-# COMPLETE Natural7 #-}

pattern Natural8 :: (forall s t u v w x y z. a s t u v w x y z -> b s t u v w x y z) -> (a ~> b)
pattern Natural8 { runNatural8 } <- ((
    runNatural7 . runNatural_ :: (a ~> b) -> (forall s t u v w x y z. a s t u v w x y z -> b s t u v w x y z)
  ) -> runNatural8) 
  where Natural8 f = Natural_ (Natural7 f)
{-# COMPLETE Natural8 #-}

pattern Natural9 :: (forall r s t u v w x y z. a r s t u v w x y z -> b r s t u v w x y z) -> (a ~> b)
pattern Natural9 { runNatural9 } <- ((
    runNatural8 . runNatural_ :: (a ~> b) -> (forall r s t u v w x y z. a r s t u v w x y z -> b r s t u v w x y z)
  ) -> runNatural9) 
  where Natural9 f = Natural_ (Natural8 f)
{-# COMPLETE Natural9 #-}

pattern Natural10 :: (forall q r s t u v w x y z. a q r s t u v w x y z -> b q r s t u v w x y z) -> (a ~> b)
pattern Natural10 { runNatural10 } <- ((
    runNatural9 . runNatural_ :: (a ~> b) -> (forall q r s t u v w x y z. a q r s t u v w x y z -> b q r s t u v w x y z)
  ) -> runNatural10) 
  where Natural10 f = Natural_ (Natural9 f)
{-# COMPLETE Natural10 #-}

pattern Natural11 :: (forall p q r s t u v w x y z. a p q r s t u v w x y z -> b p q r s t u v w x y z) -> (a ~> b)
pattern Natural11 { runNatural11 } <- ((
    runNatural10 . runNatural_ :: (a ~> b) -> (forall p q r s t u v w x y z. a p q r s t u v w x y z -> b p q r s t u v w x y z)
  ) -> runNatural11) 
  where Natural11 f = Natural_ (Natural10 f)
{-# COMPLETE Natural11 #-}

pattern Natural12 :: (forall o p q r s t u v w x y z. a o p q r s t u v w x y z -> b o p q r s t u v w x y z) -> (a ~> b)
pattern Natural12 { runNatural12 } <- ((
    runNatural11 . runNatural_ :: (a ~> b) -> (forall o p q r s t u v w x y z. a o p q r s t u v w x y z -> b o p q r s t u v w x y z)
  ) -> runNatural12) 
  where Natural12 f = Natural_ (Natural11 f)
{-# COMPLETE Natural12 #-}

pattern Natural13 :: (forall n o p q r s t u v w x y z. a n o p q r s t u v w x y z -> b n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural13 { runNatural13 } <- ((
    runNatural12 . runNatural_ :: (a ~> b) -> (forall n o p q r s t u v w x y z. a n o p q r s t u v w x y z -> b n o p q r s t u v w x y z)
  ) -> runNatural13) 
  where Natural13 f = Natural_ (Natural12 f)
{-# COMPLETE Natural13 #-}

pattern Natural14 :: (forall m n o p q r s t u v w x y z. a m n o p q r s t u v w x y z -> b m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural14 { runNatural14 } <- ((
    runNatural13 . runNatural_ :: (a ~> b) -> (forall m n o p q r s t u v w x y z. a m n o p q r s t u v w x y z -> b m n o p q r s t u v w x y z)
  ) -> runNatural14) 
  where Natural14 f = Natural_ (Natural13 f)
{-# COMPLETE Natural14 #-}

pattern Natural15 :: (forall l m n o p q r s t u v w x y z. a l m n o p q r s t u v w x y z -> b l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural15 { runNatural15 } <- ((
    runNatural14 . runNatural_ :: (a ~> b) -> (forall l m n o p q r s t u v w x y z. a l m n o p q r s t u v w x y z -> b l m n o p q r s t u v w x y z)
  ) -> runNatural15) 
  where Natural15 f = Natural_ (Natural14 f)
{-# COMPLETE Natural15 #-}

pattern Natural16 :: (forall k l m n o p q r s t u v w x y z. a k l m n o p q r s t u v w x y z -> b k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural16 { runNatural16 } <- ((
    runNatural15 . runNatural_ :: (a ~> b) -> (forall k l m n o p q r s t u v w x y z. a k l m n o p q r s t u v w x y z -> b k l m n o p q r s t u v w x y z)
  ) -> runNatural16) 
  where Natural16 f = Natural_ (Natural15 f)
{-# COMPLETE Natural16 #-}

pattern Natural17 :: (forall j k l m n o p q r s t u v w x y z. a j k l m n o p q r s t u v w x y z -> b j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural17 { runNatural17 } <- ((
    runNatural16 . runNatural_ :: (a ~> b) -> (forall j k l m n o p q r s t u v w x y z. a j k l m n o p q r s t u v w x y z -> b j k l m n o p q r s t u v w x y z)
  ) -> runNatural17) 
  where Natural17 f = Natural_ (Natural16 f)
{-# COMPLETE Natural17 #-}

pattern Natural18 :: (forall i j k l m n o p q r s t u v w x y z. a i j k l m n o p q r s t u v w x y z -> b i j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural18 { runNatural18 } <- ((
    runNatural17 . runNatural_ :: (a ~> b) -> (forall i j k l m n o p q r s t u v w x y z. a i j k l m n o p q r s t u v w x y z -> b i j k l m n o p q r s t u v w x y z)
  ) -> runNatural18) 
  where Natural18 f = Natural_ (Natural17 f)
{-# COMPLETE Natural18 #-}

pattern Natural19 :: (forall h i j k l m n o p q r s t u v w x y z. a h i j k l m n o p q r s t u v w x y z -> b h i j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural19 { runNatural19 } <- ((
    runNatural18 . runNatural_ :: (a ~> b) -> (forall h i j k l m n o p q r s t u v w x y z. a h i j k l m n o p q r s t u v w x y z -> b h i j k l m n o p q r s t u v w x y z)
  ) -> runNatural19) 
  where Natural19 f = Natural_ (Natural18 f)
{-# COMPLETE Natural19 #-}

pattern Natural20 :: (forall g h i j k l m n o p q r s t u v w x y z. a g h i j k l m n o p q r s t u v w x y z -> b g h i j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural20 { runNatural20 } <- ((
    runNatural19 . runNatural_ :: (a ~> b) -> (forall g h i j k l m n o p q r s t u v w x y z. a g h i j k l m n o p q r s t u v w x y z -> b g h i j k l m n o p q r s t u v w x y z)
  ) -> runNatural20) 
  where Natural20 f = Natural_ (Natural19 f)
{-# COMPLETE Natural20 #-}

pattern Natural21 :: (forall f g h i j k l m n o p q r s t u v w x y z. a f g h i j k l m n o p q r s t u v w x y z -> b f g h i j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural21 { runNatural21 } <- ((
    runNatural20 . runNatural_ :: (a ~> b) -> (forall f g h i j k l m n o p q r s t u v w x y z. a f g h i j k l m n o p q r s t u v w x y z -> b f g h i j k l m n o p q r s t u v w x y z)
  ) -> runNatural21) 
  where Natural21 f = Natural_ (Natural20 f)
{-# COMPLETE Natural21 #-}

pattern Natural22 :: (forall e f g h i j k l m n o p q r s t u v w x y z. a e f g h i j k l m n o p q r s t u v w x y z -> b e f g h i j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural22 { runNatural22 } <- ((
    runNatural21 . runNatural_ :: (a ~> b) -> (forall e f g h i j k l m n o p q r s t u v w x y z. a e f g h i j k l m n o p q r s t u v w x y z -> b e f g h i j k l m n o p q r s t u v w x y z)
  ) -> runNatural22) 
  where Natural22 f = Natural_ (Natural21 f)
{-# COMPLETE Natural22 #-}

pattern Natural23 :: (forall d e f g h i j k l m n o p q r s t u v w x y z. a d e f g h i j k l m n o p q r s t u v w x y z -> b d e f g h i j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural23 { runNatural23 } <- ((
    runNatural22 . runNatural_ :: (a ~> b) -> (forall d e f g h i j k l m n o p q r s t u v w x y z. a d e f g h i j k l m n o p q r s t u v w x y z -> b d e f g h i j k l m n o p q r s t u v w x y z)
  ) -> runNatural23) 
  where Natural23 f = Natural_ (Natural22 f)
{-# COMPLETE Natural23 #-}

pattern Natural24 :: (forall c d e f g h i j k l m n o p q r s t u v w x y z. a c d e f g h i j k l m n o p q r s t u v w x y z -> b c d e f g h i j k l m n o p q r s t u v w x y z) -> (a ~> b)
pattern Natural24 { runNatural24 } <- ((
    runNatural23 . runNatural_ :: (a ~> b) -> (forall c d e f g h i j k l m n o p q r s t u v w x y z. a c d e f g h i j k l m n o p q r s t u v w x y z -> b c d e f g h i j k l m n o p q r s t u v w x y z)
  ) -> runNatural24) 
  where Natural24 f = Natural_ (Natural23 f)
{-# COMPLETE Natural24 #-}
