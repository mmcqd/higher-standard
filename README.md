# Higher-Kinded
Higher kinded polymorphism (kinda) in SML!

An attempt at translating a little bit of https://github.com/ocamllabs/higher into SML.

Higher kinded polymorphism is implemented through defunctionalization. Instead of promoting type variables to
allow abstraction over type constructors (anything of kind k -> * ), we _demote_ type constructor application 
with the type `('x,'f) app`, which represents `'a 'f`. In more concrete terms: `(int,list) app == int list`. To achieve this, we make `('a,'f) app` abstract, and then provide functors to to construct a defunctionalized type constructor for a given real type constructor. These are Higher.Mk1 and Higher.Mk2, which can be easily generalized to MkN if desired. A central use of abstracting over type constructors is the ability to express haskell style typeclass constraints on them. Where the original paper used ocaml objects to represent typeclasses and their associated dictionaries, we have no choice but to use records. Ideally we could express a haskell functor like so (functor is a keyword, so we use covfunctor, for covariant functor):
```sml
type 'f covfunctor = { fmap : 'a 'b . ('a -> 'b) -> ('a,'f) app -> ('b,'f) app }
```
Unfortunately, SML has no first class polymorphism. All type variables must be bound at the site of a declaration, so we have to do this:
```sml
type ('a,'b,'f) covfunctor = { fmap : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app }
```
This is inideal, since it prevents us from using functor instaces fully polymorphically. For instance:
```sml
(*  won't_work : ('a,'b,'f) covfunctor -> (int,'f) app -> (int,'f) app *) 
fun won't_work (cls : ('a,'b,'f) covfunctor) f = #fmap cls (fn x => x+1) f
```
This fails to type check because `cls` is _not_ polymorphic in the body of `won't_work`, since `'a` and `'b` are bound at the declaration of the function, not inside the argument. We could manually instantiate 'a and 'b like so:
```sml
(*  will_work : (int,int,'f) covfunctor -> (int,'f) app -> (int,'f) app *)
fun will_work (cls : (int,int,'f) covfunctor) f = #fmap cls (fn x => x+1) f
```
But we're still limited to only mapping over structures containing ints. `Unsafe.cast` to the rescue!
```sml
(*  actually_polymorphic : ('_x,'_x,'f) covfunctor -> (int,'f) app -> (int,'f) app * (string,'f) app
fun actually_polymorphic (cls : ('_x,'_x,'f) covfunctor) f =
  let val ('a,'b) cls : ('a,'b,'f) covfunctor = Unsafe.cast cls in
  (#fmap cls (fn x => x+1) f, #fmap cls (fn x => "nice") f)
  end
```
The `'_x` business is quite janky. Basically these type variables are meant to be ignored, but we can't leave their positions blank. The `('a,'b) cls` syntax lets us bind brand new type variables at our new `cls` declaration, so we throw away the `'_x`s, and force SML to accept the covfunctor instance as being fully polymorphic. Of course, this is all completely un-typesafe, and if you pass a covfunctor instance that can't actually work polymorphically, this could fail at runtime. Check out the code for definitions of functor, applicative and monad, and a couple simple functions using type class constraints.
