structure Lens =
struct
  open TypeClass
  infixr $ 
  
  datatype ('a,'b) const = Const of 'b
  fun getConst (Const x) = x
  
  structure ConstH = Higher.Mk2 (type ('a,'b) t = ('a,'b) const)

  datatype 'a id = Id of 'a
  fun getId (Id x) = x
  
  structure IdH = Higher.Mk1 (type 'a t = 'a id) 

  val functorConst = fn () => Functor.into
    { fmap = fn f => fn x => x }

  val functorId = fn () => Functor.into
    { fmap = fn f => fn x => case IdH.out x of Id y => IdH.into $ Id $ f y }

  type ('s,'t,'a,'b,'f) lens = 
    'f Functor.cls -> ('a -> ('b,'f) app) -> 's -> ('t,'f) app


  val view : ('s,'t,'a,'b,('a,ConstH.s) app) lens -> 's -> 'a =
    fn l => getConst o ConstH.out o l (functorConst()) (ConstH.into o Const)

  val over : ('a -> 'b) -> ('s,'t,'a,'b,IdH.s) lens -> 's -> 't =
    fn f => fn l => getId o IdH.out o l (functorId()) (IdH.into o Id o f)

  fun set x = over (Fn.const x)

  val L_1 : ('a * 'x,'b * 'x,'a,'b,'f) lens =
    fn cls => fn f => fn (a,x) => Functor.prj#fmap cls (fn b => (b,x)) (f a)
  
  val L_2 : ('x * 'a,'x * 'b,'a,'b,'f) lens =
    fn cls => fn f => fn (x,a) => Functor.prj#fmap cls (fn b => (x,b)) (f a) 

  val 3 = view L_1 (3,4)
  val 4 = view L_2 (3,4)
  val ("3",4) = over Int.toString L_1 (3,4)
  val (3,5) = set 5 L_2 (3,4)

end
