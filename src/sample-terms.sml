structure Sample_Terms =
struct
  (* s(s(z)) *)
  val term1 : Term.t = Term.$$(TermOps.Succ, [Term.$$(TermOps.Zero, [])])

  (* val term2 = Term.$$(TermOps.Lam, *)
                      (* [Term.\\ (Var.newvar "x", *)
                                (* Term.$$(TermOps.Zero, []))]) *)
end
