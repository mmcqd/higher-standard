# Higher-Kinded
Higher kinded polymorphism (sort of) in SML!

An attempt at translating a little bit of https://github.com/ocamllabs/higher into SML.

Higher kinded polymorphism is implemented through defunctionalization. Instead of promoting type variables to
allow abstraction over type constructors (anything of kind k -> * ), we _demote_ type constructor application 
with the type `('x,'f) app`, which represents `'a 'f`. In more concrete terms: `(int,list) app == int list`. To achieve this, we make `('a,'f) app` abstract, and then provide functors to to construct a defunctionalized type constructor for a given real type constructor. These are Higher.Mk1 and Higher.Mk2, which can be easily generalized to MkN if desired. 

**Rewrite of README in progress**
