extend Dual into a Flip family
extend Iso into an Iso family
Identity/Const/etc also need proofs of coercibility
decide whether to continue with eliminate
Void, Unit as Const

```haskell
type family Void = (v :: k) | v -> k where
  Void = Shadowed.Void
  Void = Const Void
```

better names/fewer underscores ? :)

define up to arity ...7? 10?

Questions:

Lessons learned:
- extending an existing type into a data family w/ injective type families
- argumentless type families (hidden kind arguments)
- Free
- poly-kinded data families
- newtype families by including Coercion
- lack of roles for data families and eliminate

incorporate old code pt 1

```haskell

fstUnit :: (Exponential m, Cartesian m, Final m) => Iso m (Product m (Terminal m) a) a
fstUnit = Iso snd (unit &&& id)

_Exp :: (Exponential m, Cartesian m, Final m) => Iso m (Product m (Terminal m) a) a
_Exp :: forall (a :: k) b.  (Final m, Exponential m

(a `m` b) <-> Terminal m `m` (Exp m b a)
_Exp = Iso
  (\f -> to _Curry $ f . to lunit)
  (\f -> from _Curry f . from lunit)

type Select = (~>) Unit
lunit :: forall (a :: k). Concrete k => Unit × a <~> a
lunit = Iso snd (unit &&& id)

type Select = (~>) Unit

_Exp :: forall (a :: k) b. Concrete k => (a ~> b) <-> Select (b^a)
_Exp = Iso
  (\f -> to _Curry $ f . to lunit)
  (\f -> from _Curry f . from lunit)

productUniversality :: forall (a :: k) b c. Concrete k => Select (a^c × b^c) <-> Select ((a × b)^c)
productUniversality = Iso
  (\p -> to _Exp (from _Exp (fst . p) &&& from _Exp (snd . p)))
  (\(from _Exp -> p) -> (to _Exp $ fst . p) &&& (to _Exp $ snd . p))

coproductUniversality :: forall (a :: k) b c. Concrete k => Select (c^a × c^b) <-> Select (c^(a + b))
coproductUniversality = Iso
  (\p -> to _Exp (from _Exp (fst . p) ||| from _Exp (snd . p)))
  (\(from _Exp -> p) -> (to _Exp $ p . inl) &&& (to _Exp $ p . inr))
```
