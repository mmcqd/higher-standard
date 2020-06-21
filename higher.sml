structure Higher :> HIGHER =
struct
  type ('x,'f) app = unit
  
  structure B =
  struct
    val into = Unsafe.cast
    val out  = Unsafe.cast
   end

  functor Mk1 (type 'a t) =
  struct
    type s = unit
    open B
  end

  functor Mk2 (type ('a,'b) t) =
  struct
    type s = unit
    open B
  end

  functor MkCls (type ('a,'b,'c,'d,'e,'P) base) =
  struct
    type 'P cls = unit
    open B
    fun prj f c = f (out c)
  end

end




