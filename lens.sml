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

  val over : ('s,'t,'a,'b,IdH.s) lens -> ('a -> 'b) -> 's -> 't =
    fn l => fn f => getId o IdH.out o l (functorId()) (IdH.into o Id o f)

  fun set l x = over l (Fn.const x)


  (* We can't compose lenses directly because of explicit instance passing,
   * but we can almost do it *)
  val % = fn (a,b) => fn c => (a c) o (b c)
  infix %

  val lens : ('s -> 'a) -> ('s -> 'b -> 't) -> ('s,'t,'a,'b,'f) lens =
     fn get => fn set => fn cls => fn f => fn x =>
       Functor.prj#fmap cls (set x) (f (get x))
  
  val L_1 : ('a * 'x,'b * 'x,'a,'b,'f) lens =
    fn x => lens (fn (a,_) => a) (fn (_,x) => fn b => (b,x)) x

  val L_2 : ('x * 'a,'x * 'b,'a,'b,'f) lens =
    fn x => lens (fn (_,a) => a) (fn (x,_) => fn b => (x,b)) x
      

  val digit_to_int = fn x => Char.ord x - Char.ord #"0"

  val to_d : int -> int list =
    List.map digit_to_int o String.explode o Int.toString

  val from_d : int list -> int =
    foldl (fn (x,xs) => xs*10+x) 0

  val L_digit : (int list,int list,int,int,'f) lens =
    fn x => lens from_d (Fn.const to_d) x

  val 3 = view L_1 (3,4)
  val 4 = view L_2 (3,4)
  val ("3",4) = over L_1 Int.toString (3,4)
  val (3,5) = set L_2 5 (3,4)
  val 3 = view (L_1 % L_1) ((3,4),5)

end
