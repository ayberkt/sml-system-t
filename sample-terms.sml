(* s(s(z)) *)
val term1 : Term.t =
    Term.$$(TermOps.Succ, [Term.$$(TermOps.Zero, [])])

(* lam{nat}(x.z) *)
val term2 : Term.t =
    Term.$$(TermOps.Lam (Type.$$(TypeOps.NAT, [])),
            [Term.\\ (Var.newvar "x",
                      Term.$$(TermOps.Zero, []))])

(* (lam{nat}(x.s(x))) s(s(z)) *)
val term3 : Term.t =
    Term.$$(TermOps.App, [term2, term1])

val term4 : Term.t =
    let
        val x1 = Var.newvar "x"
        val x2 = Var.newvar "y"
        val z : Term.t = Term.$$(TermOps.Zero, [])
        val body : Term.t = Term.\\ (x1,
                                     Term.\\ (x2,
                                              Term.$$(TermOps.Succ,
                                                      [Term.`` x2])))
    in
        Term.$$(TermOps.Rec, [z, body, term1])
    end

val _ =
    let
        val nat = (Type.$$(TypeOps.NAT, []))
        fun printType (term : Term.t) : unit =
          let
              val termStr = Term.toString term
              val ty = TypeChecker.typecheck Context.empty term
              val tyStr = Type.toString ty
          in
              print (termStr ^ " : " ^ tyStr ^ "\n")
          end
    in
        List.map printType [term1, term2, term3, term4]
    end
