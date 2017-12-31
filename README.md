_This is not an introduction on how to use the library; please consult [the Haddocks](https://hackage.haskell.org/package/explicit-constraint-lens/docs/Lens-Explicit.html) for that. The following assumes some familiarity with the general idea of lenses and discusses differences between this library and others._
<hr>

`explicit-constraint-lens` is a library for generalised lenses (aka labels, functional references, accessors...)
in general called _optics_, i.e. it offers ways to access and/or update parts of data structures in a composable manner.

The design goals are, by order of importance:

1. **No restrictions**. How powerful lenses can be is manifested by the
 [`lens`](http://hackage.haskell.org/package/lens) package, which offers a fine-grained hierachy from extremely strong `Iso`morphisms to very weak (and thus general) `Fold`s and `Setter`s. All these different kinds of optics are perfectly compatible, as far as this is mathematically possible, and transformations can change the type of the data structure.
 Here we implement that hierachy in full, which most other libraries such as [`fclabels`](http://hackage.haskell.org/package/fclabels) and [`data-accessor`](http://hackage.haskell.org/package/data-accessor) do not. (Which does _not_ mean we offer all the _features_ of `lens` here, just give a framework on which all the functionality can be implemented.)
2. **Simple and clean interface**. `lens` is often criticised for being obscure, with its Var Laarhoven universally-quantified constructions such as
 <pre>type <a href="http://hackage.haskell.org/package/lens-4.15.4/docs/Control-Lens-Iso.html#t:Iso">Iso</a> s t a b = ∀ p f. (<a href="https://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor-Types.html#t:Profunctor">Profunctor</a> p, <a href="http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Functor.html#t:Functor">Functor</a> f) => p a (f b) -> p s (f t) </pre>
 In this library, it is merely
 <pre>type <a href="https://hackage.haskell.org/package/explicit-constraint-lens/docs/Lens-Explicit-Core.html#t:Iso">Iso</a> s t a b = ∀ c. <a href="https://hackage.haskell.org/package/explicit-constraint-lens/docs/Lens-Explicit-Core.html#t:FromIso">FromIso</a> c => <a href="https://hackage.haskell.org/package/explicit-constraint-lens/docs/Lens-Explicit-Core.html#t:Optic">Optic</a> c s t a b</pre>
3. **No conflict with the function data-type**. Van Laarhoven lenses are just functions, albeit functions that operate in a quite surprising way and in often seemingly “in reverse direction” (via CPS). That is arguably quite elegant, but it also causes problems; in particular, the type system can not clearly distinguish between a “plain old function” and a function that's actually a lens. This is a dilemma for the future of Haskell's records, which most agree are in need of a completely new approach, to avoid the problems of name clashes and awkward special syntax. This is adressed by the [Overloaded Record Fields](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst). But many Haskellers think that lenses are the way to go and should completely replace traditional accessors, yet many others have concerns about difficulty etc..<br>If functions and lenses can be distinguished by the type system, there is no need to decide up-front: everybody can just pick what they like better.
4. **Lightwight**. `lens` is “batteries included”, and takes quite long to build with its extensive dependecies. In its defense, the ingenuity of the Van Laarhoven signatures is that one can define ordinary lenses in a compatible manner _without_ actually using the lens package.
 The [`microlens` package](http://hackage.haskell.org/package/microlens) therefore offers a minimal basis on which to start out. Still, it then has those signatures that are certainly not beginner-friendly, in particular when it comes to error messages, and it can not offer the full optics hierarchy – you need lots of strange category-theory classes to do that in Van Laarhoven style.  
 <br>This library, by contrast, has no dependencies whatsoever and implements the full hierarchy in a single 240 LOC module, plus another 200 LOC for the most important operators and documentation.

These goals are accomplished by having
- for each flavour of optic a small dedicated type class with descriptive, consistent naming and the appropriate superclasses
- a common wrapper `OpticC` that links it all together and offers a [`Category`](http://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Category.html#t:Category) instance which allows our optics to be composed quite as well as the ones from `lens`.

The optics are not directly compatible to Van Laarhoven signatures, but conversion is not difficult.

<hr>

These ideas aren't new, really. In fact, Twan van Laarhoven suggested right in [his first blog post on lenses](https://www.twanvl.nl/blog/haskell/overloading-functional-references) <sup>(well before [the one that introduced what would later be called &ldquo;Van Laarhoven lenses&rdquo;](https://www.twanvl.nl/blog/haskell/cps-functional-references))</sup> simply making a class
```
class Ref r where
      ref :: (a -> b) -> (b -> a -> a) -> r a b
```
which does largely the same as our
<pre>class FromIso c => <a href="https://hackage.haskell.org/package/explicit-constraint-lens-0.1.0.0/docs/Lens-Explicit-Core.html#t:FromLens">FromLens</a> c where<br>  lens :: (s -> a) -> (s -> b -> t) -> c s t a b</pre>
Yet no prior library I'm aware of seems to have consequently taken this route.
