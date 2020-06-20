signature HIGHER =
sig
  type ('x,'f) app
  
  functor Mk1 (type 'a t) :
  sig
    type s
    val into : 'a t -> ('a,s) app
    val out  : ('a,'s) app -> 'a t
  end

  functor Mk2 (type ('a,'b) t) :
  sig
    type s
    val into : ('a,'b) t -> ('a,('b, s) app) app
    val out  : ('a, ('b,s) app) app -> ('a,'b) t
  end
end



structure Higher :> HIGHER =
struct
  type ('x,'f) app = unit
  
  functor Mk1 (type 'a t) =
  struct
    type s = unit
    val into = Unsafe.cast
    val out  = Unsafe.cast
  end

  functor Mk2 (type ('a,'b) t) =
  struct
    type s = unit
    val into = Unsafe.cast
    val out  = Unsafe.cast
  end

end

fun $ (f,x) = f x
infixr $

open Either

type ('a,'b) app = ('a,'b) Higher.app

structure ListH = Higher.Mk1 (type 'a t = 'a list)
structure OptionH = Higher.Mk1 (type 'a t = 'a option)
structure EitherH = Higher.Mk2 (type ('a,'b) t = ('a,'b) either)

type ('a,'b,'f) covfunctor = {fmap : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app}

type ('a,'b,'f) applicative = 
  { fmap : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app,
    pure : 'a -> ('a,'f) app, 
    ap   : ('a -> 'b, 'f) app -> ('a,'f) app -> ('b,'f) app
  }

type ('a,'b,'m) monad =
  { fmap : ('a -> 'b) -> ('a,'m) app -> ('b,'m) app,
    pure : 'a -> ('a,'m) app,
    ap   : ('a -> 'b,'m) app -> ('a,'m) app -> ('b,'m) app,
    bind : ('a,'m) app -> ('a -> ('b,'m) app) -> ('b,'m) app
  }

val covfunctorList : ('a,'b,ListH.s) covfunctor =
  {fmap = fn f => ListH.into o List.map f o ListH.out}

val monadOption : ('a,'b,OptionH.s) monad =
           { fmap = fn f => OptionH.into o Option.map f o OptionH.out,
             pure = fn x => OptionH.into o SOME $ x,
             ap   = (fn f => fn x =>
                      case (OptionH.out f,OptionH.out x) of
                         (SOME f, SOME x) => OptionH.into o SOME $ f x
                       | _                => OptionH.into NONE),
             bind = (fn x => fn f =>
                      case OptionH.out x of
                        SOME x => f x
                      | _      => OptionH.into NONE) 
           }


val monadEither : ('a,'b, ('c,EitherH.s) app) monad =
           { fmap = (fn f => fn x => 
                      case EitherH.out x of
                        INL y => EitherH.into o INL $ f y
                      | INR y => EitherH.into $ INR y),
             pure = fn x => EitherH.into o INL $ x,
             ap   = (fn f => fn x =>
                      case (EitherH.out f,EitherH.out x) of
                        (INL f,INL x) => EitherH.into o INL $ f x
                      | (INL _,INR x) => EitherH.into $ INR x 
                      | (INR f,_)     => EitherH.into $ INR f),
             bind = (fn x => fn f =>
                      case EitherH.out x of
                        INL x => f x
                      | INR x => EitherH.into $ INR x)
                      
           }

val monadList : ('a,'b,ListH.s) monad =
           { fmap = fn x => #fmap covfunctorList x,
             pure = fn x => ListH.into [x],
             ap   = fn x => let
                      fun ap fs xs = 
                        case ListH.out fs of
                          []    => ListH.into []
                        | f::fs => ListH.into (List.map f (ListH.out xs) @ (ListH.out $ ap (ListH.into fs) xs))
                     in ap end x,
             bind = fn x => fn f => ListH.into $ List.concatMap (ListH.out o f) (ListH.out x)
            }



fun <$ (cls : ('_x,'_x,'f) covfunctor) x f = 
  let val ('a,'b) cls : ('a,'b,'f) covfunctor = Unsafe.cast cls in
  #fmap cls (Fn.const x) f
  end

fun classes_can_be_truly_polymorphic (cls : ('_x,'_x,'f) covfunctor) f =
  let val ('a,'b) cls : ('a,'b,'f) covfunctor = Unsafe.cast cls in
  (#fmap cls (fn x => x+1) f,<$ cls #"w" f)
  end

fun addM (cls : ('_x,'_x,'m) monad) a b =
  let val ('a,'b) cls : ('a,'b,'m) monad = Unsafe.cast cls in
  #bind cls a (fn x =>
  #bind cls b (fn y =>
    #pure cls $ x+y))
  end

fun on (h,f) x y = h (f x) (f y)
infix on

val SOME 3 = OptionH.out $ (addM monadOption on OptionH.into) (SOME 1) (SOME 2)
val NONE   = OptionH.out $ (addM monadOption on OptionH.into) NONE (SOME 2)




