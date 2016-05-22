structure Sample_Terms =
struct
  (* s(s(z)) *)
  val term1 : Term.t = Term.$$(TermOps.Succ, [Term.$$(TermOps.Zero, [])])

  val term2 : Term.t = Term.$$(TermOps.Lam (Type.$$(TypeOps.NAT, [])),
                               [Term.\\ (Var.newvar "x",
                                         Term.$$(TermOps.Zero, []))])
end
