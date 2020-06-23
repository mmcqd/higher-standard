structure Examples =
struct
  open TypeClass
  infixr $
  
  fun <$ cls x = let open Functor in 
    &cls #fmap (Fn.const x)
    end

  fun classes_are_actually_polymorphic cls f = let open Functor in
    (&cls #fmap (fn x => x+1) f,<$ cls #"w" f)
    end

  fun addM cls a b = let open Monad in
    &cls #bind a (fn x =>
    &cls #bind b (fn y =>
      &cls #pure $ x+y))
    end
  
  fun when cls b m = if b then m else Monad.&cls #pure ()
  fun unless cls b m = when cls (not b) m

  fun on (h,f) x y = h (f x) (f y)
  infix on

  val SOME 3 = OptionH.out $ (addM (monadOption()) on OptionH.into) (SOME 1) (SOME 2)
  val NONE   = OptionH.out $ (addM (monadOption()) on OptionH.into) NONE (SOME 2)
  val [2,3,4,3,4,5,4,5,6] = ListH.out $ (addM (monadList()) on ListH.into) [1,2,3] [1,2,3]
end
