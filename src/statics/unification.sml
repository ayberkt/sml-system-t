structure Unification =
struct
  exception DoesNotUnify
  !! = StringDict.lookup
  infixr 4 !!

  type subst = Fol.Term StringDict.map

  fun indom (x : int) (eqns : subst) : bool =
    StringDict.exists (op= x) eqns

  (*fun app (((y, t)::ss) : subst) (x : int) : Fol.term =*)
    (*if x = y then t else app s x*)

end
