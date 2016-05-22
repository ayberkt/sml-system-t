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

        (* lam{nat}(x.s(x)) *)
        val term2 : Term.t =
            Term.$$(TermOps.Lam (Type.$$(TypeOps.NAT, [])),
                    [Term.\\ (Var.newvar "x",
                              Term.$$(TermOps.Zero, []))])

    in
        printType term1;
        printType term2
    end
