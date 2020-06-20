# Higher Standard
Higher kinded polymorphism in Standard ML!

An attempt at translating some of https://github.com/ocamllabs/higher into SML.

Higher kinded polymorphism is implemented through defunctionalization. Instead of promoting type variables to
allow abstraction over type constructors (anything of kind k -> * ), we _demote_ type constructor application 
with the type `('x,'f) app`, which represents `'a 'f`. In more concrete terms: `(int,list) app == int list`. To achieve this, we make `('a,'f) app` abstract, and then provide functors to to construct a defunctionalized type constructor for a given real type constructor. These are Higher.Mk1 and Higher.Mk2, which can be easily generalized to MkN if desired. A central use case of abstracting over type constructors is allowing haskell style type class constraints on them. The original paper uses the ocaml object system to represent type classes, but SML has no such system. It does however have records. Ideally, we'd like to be able to do this:
```sml
type 'f Functor = { fmap : forall 'a 'b . ('a -> 'b) -> ('a,'f) app -> ('b,'f) app }
```
Unfortunately, SML lacks any first class polymorphism, so all type variable must be bound at the site of the type declaration, and not inside the record field. This results in the rather unsatisfying
```sml
type ('a,'b,'f) Functor = { fmap : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app }
```
Not only does this not look as nice, it causes issues when try to use type class instances. For example:
```sml
(*  won't_work : ('a,'b,'f) Functor -> (int,'f) app -> (int,'f) app *) 
fun won't_work (cls : ('a,'b,'f) Functor) f = #fmap cls (fn x => x+1) f
```
This fails to type check because `cls` is _not_ polymorphic in the body of `won't_work`, since `'a` and `'b` are bound at the declaration of the function, not inside the argument. To alleviate this, we provide the `MkCls` functor:
```sml
functor MkCls (type ('a,'b,'c,'d,'e,'P) base) :
sig
  type 'P cls
  val into : ('a,'b,'c,'d,'e,'P) base -> 'P cls
  val out  : 'P cls -> ('a,'b,'c,'d,'e,'P) base
  val prj  : (('a,'b,'c','d,'e,'P) base -> 'f) -> 'P cls -> 'f
end
```
Unfortunately this can be used to create objects that don't act like type classes, but it's meant to be use like so:
```sml
structure Functor = 
  Higher.MkCls (type ('a,'b,'c,'d,'e,'f) base = 
    { fmap : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app }
  )
```
The first 5 type variables are intended for use anywhere in your type class, but you don't have to use all of them. The final `'f` is intended to be the type the class is constraining. If you need more than 5 type variables to write your class, the code can be rewritten to use as many as you like. Now `Functor.into` turns a record into a Functor instance, whose fields we can access with `Functor.prj`, i.e. `Functor.prj#fmap instance`. This allows writing fairly idiomatic functions with type class arguments:
```sml
(*  <$ : 'f Functor.cls -> 'a -> ('b,'f) app -> ('a,'f) app *)
fun <$ cls x = Functor.prj#fmap cls (const x)
```

To use the `when` and `unless` examples from the original paper, assuming we've created a monad class:
```sml
(*  when : 'm Monad.cls -> bool -> (unit,'m) app -> (unit,'m) app *)
fun when cls b m = if b then m else Monad.prj#pure cls ()

(*  unless : 'm Monad.cls -> bool -> (unit,'m) app -> (unit,'m) app *)
fun unless cls b m = when cls (not b) m
```

