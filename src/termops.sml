structure TermOps =
struct

  datatype t =
    Zero
  | Succ
  | Rec
  | Lam of Type.t
  | App

  fun arity Zero     = []
    | arity Succ     = [0]
    | arity Rec      = [0, 2, 0]
    | arity (Lam _)  = [1]
    | arity App      = [0, 0]

  fun equal (Zero, Zero)     = true
    | equal (Succ, Succ)     = true
    | equal (Rec,  Rec)      = true
    | equal (Lam t1,  Lam t2) = Type.aequiv(t1, t2)
    | equal (App,  App)      = true
    | equal _                = false

  fun toString Zero    = "z"
    | toString Succ    = "s"
    | toString Rec     = "rec"
    | toString (Lam t) = "lam" ^ (Type.toString t)
    | toString App     = "ap"

end
