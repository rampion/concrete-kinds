extend Dual into a Flip family
extend Iso into an Iso family
replace ReprEq with Data.Type.Coercion
add call / interpret to Free

better names/fewer underscores ? :)

define up to arity ...7? 10?

Questions:
- eliminate is evil, but does it work?
- is eliminate necessary?
- do Identity/Const/etc also need proofs of coercibility?

Lessons learned:
- extending an existing type into a data family w/ injective type families
- argumentless type families (hidden kind arguments)
- Free
- poly-kinded data families
- newtype families by including Coercion
