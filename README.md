This package defines *concrete kinds* as the family of kinds for
type constructors defined by `data` and `newtype`.

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
