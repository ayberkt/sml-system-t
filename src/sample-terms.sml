val _ =
    let
        fun printType (term : Term.t) : unit =
          let
              val termStr = Term.toString term
              val ty = TypeChecker.typecheck Context.empty term
              val tyStr = Type.toString ty
          in
              print (termStr ^ " : " ^ tyStr ^ "\n")
          end
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

    in
        List.map printType [term1, term2, term3]
    end
