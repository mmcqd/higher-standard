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

  functor MkCls (type ('a,'b,'c,'d,'e,'P) base) :
  sig
    type 'P cls
    val into : ('a,'b,'c,'d,'e,'P) base -> 'P cls
    val out  : 'P cls -> ('a,'b,'c,'d,'e,'P) base
    val prj  : (('a,'b,'c','d,'e,'P) base -> 'f) -> 'P cls -> 'f
  end
end
