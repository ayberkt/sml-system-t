structure TermOps : OPERATOR =
struct

  datatype t =
    Zero
  | Succ
  | Rec
  | Lam
  | App

  fun arity Zero = []
    | arity Succ = [0]
    | arity Rec  = [0, 2, 0]
    | arity Lam  = [1]
    | arity App  = [0, 0]

  fun equal (Zero, Zero) = true
    | equal (Succ, Succ) = true
    | equal (Rec,  Rec)  = true
    | equal (Lam,  Lam)  = true
    | equal (App,  App)  = true
    | equal (_, _)       = false

  fun toString Zero = "z"
    | toString Succ = "s"
    | toString Rec = "rec"
    | toString Lam = "lam"
    | toString App = "ap"

end
