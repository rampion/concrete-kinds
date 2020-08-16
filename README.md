This package defines *concrete kinds* as the family of kinds for
type constructors for instantiable types, that is types defined by `data` and
`newtype`.

Kinds are the "types of types". [From the haskell wiki](https://wiki.haskell.org/Kind):

> Ordinary types, also called monotypes or nullary type constructors, have kind
> `*`. Higher order type constructors have kinds of the form `P -> Q`, where `P`
> and `Q` are kinds. For instance:
> 
> ```haskell
> Int :: *
> Maybe :: * -> *
> Maybe Bool :: *
> a -> a :: *
> [] :: * -> *
> (->) :: * -> * -> *
> ```
> 
> A type with a more complicated kind is the `StateT` monad transformer
> 
> ```haskell
> newtype StateT s m a :: * -> (* -> *) -> * -> *
> ```
> 
> In Haskell 98, `*` is the only inhabited kind, that is, all values have types
> of kind `*`. GHC introduces another inhabited kind, `#`, for unlifted types.


Not going to get into unlifted types (`#`).


Alternative names:

  instantiable-type constructors
  CoIT - constructors of instantiable types

  cointy
  coninty
  intyp

  tycon kinds
  intyco kinds
  tyco
  tyctor
  asterisk
  star constructor kinds
  starcon kinds
  starctor
  instantiable constructor kinds
  inscon
  coin kinds

Type constructors can have an arbitrary number of arguments of arbitrary kinds,
but must always result in a type of kind `*` (aka `Type`). That is to say, 
concrete kinds can be recursively defined as:

    CONCRETE-KIND := * | k -> CONCRETE-KIND

This package provides each concrete kind with a default
[category](https://hackage.haskell.org/package/base/docs/Control-Category.html)
along with products, coproducts, exponents, an inital object, a terminal
object, functor composition, free types, and fixed points.

----

TODO: rewrite classes from `categories` instead of custom `Control.Category.*`

- Cartesian -> Cartesian
- Choice -> Monoidal arr (Coproduct Arr)
- Cocartesian -> CoCartesian
- Distributive -> Distributive
- Exponential -> Exponential
- Final -> HasTerminalObject
- Initial -> HasInitialObject
- Strong -> Monoidall arr (Product Arr)

Need to rewrite b/c categories isn't sufficiently kind-polymorphic

TODO: Rename "Natural" to "Arrow"? - see rewrite
