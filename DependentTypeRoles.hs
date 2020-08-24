module DependentTypeRoles where

    import Data.Coerce (Coercible)
    import Data.Constraint (Dict(Dict)) -- from constraints

    data Exp b a = Exp (a -> b)

    fullExp :: Coercible a b => Dict (Coercible (Exp a x) (Exp b x))
    fullExp = Dict

    partialExp :: Coercible a b => Dict (Coercible (Exp a) (Exp b))
    partialExp = Dict

    newtype Swap f b a = Swap (f a b)

    fullSwap :: Coercible a b => Dict (Coercible (Swap (->) a x) (Swap (->) b x))
    fullSwap = Dict

    {-
    partialSwap :: Coercible a b => Dict (Coercible (Swap (->) a) (Swap (->) b))
    partialSwap = Dict

    DependentTypeRoles.hs:19:19: error:
        • Couldn't match type ‘a’ with ‘b’ arising from a use of ‘Dict’
          ‘a’ is a rigid type variable bound by
            the type signature for:
              partialSwap :: forall a b.
                             Coercible a b =>
                             Dict (Coercible (Swap (->) a) (Swap (->) b))
            at DependentTypeRoles.hs:18:5-80
          ‘b’ is a rigid type variable bound by
            the type signature for:
              partialSwap :: forall a b.
                             Coercible a b =>
                             Dict (Coercible (Swap (->) a) (Swap (->) b))
            at DependentTypeRoles.hs:18:5-80
        • In the expression: Dict
          In an equation for ‘partialSwap’: partialSwap = Dict
        • Relevant bindings include
            partialSwap :: Dict (Coercible (Swap (->) a) (Swap (->) b))
              (bound at DependentTypeRoles.hs:19:5)
       |
    19 |     partialSwap = Dict
       |                   ^^^^
    -}

    data Dual f b a = Dual (f a b)

    {-
    fullDual :: Coercible a b => Dict (Coercible (Dual (->) a x) (Dual (->) b x))
    fullDual = Dict

    DependentTypeRoles.hs:50:16: error:
        • Couldn't match type ‘a’ with ‘b’ arising from a use of ‘Dict’
          ‘a’ is a rigid type variable bound by
            the type signature for:
              fullDual :: forall a b x.
                          Coercible a b =>
                          Dict (Coercible (Dual (->) a x) (Dual (->) b x))
            at DependentTypeRoles.hs:49:5-81
          ‘b’ is a rigid type variable bound by
            the type signature for:
              fullDual :: forall a b x.
                          Coercible a b =>
                          Dict (Coercible (Dual (->) a x) (Dual (->) b x))
            at DependentTypeRoles.hs:49:5-81
        • In the expression: Dict
          In an equation for ‘fullDual’: fullDual = Dict
        • Relevant bindings include
            fullDual :: Dict (Coercible (Dual (->) a x) (Dual (->) b x))
              (bound at DependentTypeRoles.hs:50:5)
       |
    50 |     fullDual = Dict
       |                ^^^^
    -}
