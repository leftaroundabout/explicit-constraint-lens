This is a lens library, i.e. it offers ways to access and/or update parts of data structures
in a composable manner. The design goals are, by order of importance:

1. **No restrictions**. The pinnacle of genericity is the
 [`lens`](http://hackage.haskell.org/package/lens) package, which offers a fine-grained hierachy from extremely strong `Iso`morphisms to very weak (and thus general) `Fold`s and `Setter`s. All these different kinds of optics are perfectly compatible, as far as this is mathematically possible, and transformations can change the type of the data structure.
 Here we implement that hierachy in full, which most other libraries such as [`fclabels`](http://hackage.haskell.org/package/fclabels) and [`data-accessor`](http://hackage.haskell.org/package/data-accessor) do not. (Which does _not_ mean we offer all the _features_ of `lens` here, just give a framework on which all the functionality to be implemented.)
2. **Simple and clean interface**. `lens` is often criticised for being obscure, with its Var Laarhoven universally-quantified constructions such as
  ```
  type Review t b = forall p f. (Choice p, Bifunctor p, Settable f) => Optic' p f t b
  ```
3. **No conflict with the function data type**. Van Laarhoven lenses are just functions, albeit functions that operate in a quite surprising way and in many cases seemingly “in reverse direction” (via CPS). That is arguably quite elegant, but it also causes problems; in particular, the type system can not clearly distinguish between a “plain old function” and a function that's actually a lens. This is a dilemma for the future of Haskell's records, which most agree are in need of a completely new approach, to avoid the problems of name clashes and awkward special syntax. This is adressed by the [Overloaded Record Fields](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst). But many Haskellers think that lenses are the way to go and should completely replace traditional accessors, yet many others have concerns about difficulty etc..<br>If functions and lenses can be distinguished by the type system, there is no need to decide up front, everybody can just pick what they like better.
4. **Lightwight**. `lens` is “batteries included”, and takes quite long to build with its extensive dependecies. In its defense, the ingenuity of the Van Laarhoven signatures is that one can define ordinary lenses in a compatible manner _without_ actually using the lens package.
  The [`microlens` package](http://hackage.haskell.org/package/microlens) therefore offers a minimal basis on which to start out. Still, it then has those signatures that are certainly not beginner-friendly, in particular when it comes to error messages, and it can not offer the full optics hierarchy – you need lots of strange category-theory classes to do that in Van Laarhoven style.

These goals are accomplished with a small dedicated type class with a descriptive name and the appropriate superclasses for each flavour of optic, and a common wrapper that links it all together and offers a [`Category`](http://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Category.html#t:Category) instance which allows our optics to be composed quite as well as the ones from `lens`.

The optics are not directly compatible to Van Laarhoven signatures, but conversion is not difficult.
