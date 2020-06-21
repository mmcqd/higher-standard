
structure TypeClass =
struct
  fun $ (f,x) = f x
  infixr $

  open Either

  type ('a,'b) app = ('a,'b) Higher.app

  structure ListH = Higher.Mk1 (type 'a t = 'a list)
  structure OptionH = Higher.Mk1 (type 'a t = 'a option)
  structure EitherH = Higher.Mk2 (type ('a,'b) t = ('a,'b) either)


  structure Functor = 
    Higher.MkCls (type ('a,'b,'c,'d,'e,'f) base = 
      { fmap : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app }
    )

  structure Applicative =
    Higher.MkCls (type ('a,'b,'c,'d,'e,'f) base =  
      { fmap : ('a -> 'b) -> ('a,'f) app -> ('b,'f) app,
        pure : 'a -> ('a,'f) app, 
        ap   : ('a -> 'b, 'f) app -> ('a,'f) app -> ('b,'f) app
      }
    )

  structure Monad =
    Higher.MkCls (type ('a,'b,'c,'d,'e,'m) base =  
      { fmap : ('a -> 'b) -> ('a,'m) app -> ('b,'m) app,
        pure : 'a -> ('a,'m) app,
        ap   : ('a -> 'b,'m) app -> ('a,'m) app -> ('b,'m) app,
        bind : ('a,'m) app -> ('a -> ('b,'m) app) -> ('b,'m) app
      }
    )


  val functorList = fn () => Functor.into
    {fmap = fn f => ListH.into o List.map f o ListH.out}

  val functorOption = fn () => Functor.into
    {fmap = fn f => OptionH.into o Option.map f o OptionH.out}

  val functorEither = fn () => Functor.into
    {fmap = fn f => fn x => 
              case EitherH.out x of
                INL y => EitherH.into o INL $ f y
              | INR y => EitherH.into $ INR y}


  val monadOption = fn () => Monad.into
    { fmap = Functor.prj#fmap $ functorOption(),
      pure = OptionH.into o SOME,
      ap   = (fn f => fn x =>
              case (OptionH.out f,OptionH.out x) of
                  (SOME f, SOME x) => OptionH.into o SOME $ f x
                | _                => OptionH.into NONE),
      bind = (fn x => fn f =>
              case OptionH.out x of
                SOME x => f x
              | _      => OptionH.into NONE) 
    }


  val monadEither = fn () => Monad.into
    { fmap = Functor.prj#fmap $ functorEither(),
      pure = EitherH.into o INL,
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

  val monadList = fn () => Monad.into
    { fmap = Functor.prj#fmap $ functorList(),
      pure = fn x => ListH.into [x],
      ap   = fn x => let
              fun ap fs xs = 
                case ListH.out fs of
                  []    => ListH.into []
                | f::fs => ListH.into (List.map f (ListH.out xs) @ (ListH.out $ ap (ListH.into fs) xs))
              in ap end x,
      bind = fn x => fn f => ListH.into $ List.concatMap (ListH.out o f) (ListH.out x)
    }
end
