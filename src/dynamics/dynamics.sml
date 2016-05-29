structure Dynamics : DYNAMICS =
struct

  exception RuntimeError
  exception Malformed

  datatype d = STEP of Term.t | VAL
  datatype D = Step of Term.t | Val | Err

  fun makeRec e0 e1 e = Term.$$(TermOps.Rec, [e0, e1, e])

  fun view d1 =
    case d1 of
        STEP t1 => Step t1
      | VAL     => Val

  fun trystep e =
    case Term.out e of
         (* ------------------------------- (9.2a) *)
         (*             z val                      *)
         Term.$(TermOps.Zero, _)          => VAL
         (* ------------------------------- (9.2c) *)
         (*          lam{τ}(x.e)  val              *)
      |  Term.$(TermOps.Lam _, [body])    => VAL
      |  Term.$(TermOps.Succ, [e])        => succ e
      |  Term.$(TermOps.App, [e1, e2])    => app e1 e2
      |  Term.$(TermOps.Rec, [e0, e1, e]) => recurse e0 e1 e
      | _ => raise Malformed
  and app (e1 : Term.t) (e2 : Term.t) : d =
    case trystep e1 of
         VAL =>
         (case trystep e2 of
              VAL =>
              (case (Term.out e1) of
                   (*             e2 val                       *)
                   (* --------------------------------- (9.3d) *)
                   (*  ap(lam{_}(x.e); e2) ↦ [e2/x] e          *)
                   Term.$(TermOps.Lam _, [e']) =>
                   (case Term.out e' of
                        Term.\ (x : Var.t, e : Term.t) =>
                        STEP(Term.subst e2 x e)
                     | _ => raise Malformed )
                 | _ => raise Malformed
              )
            (*  e1 val              e2 ↦ e2'          *)
            (* ------------------------------- (9.3c) *)
            (*     ap(e1; e2) ↦ ap(e1; e2')           *)
            | STEP(e2') => STEP(Term.$$(TermOps.App, [e1, e2']))
         )
       | STEP(e1') =>
         (*             e1 ↦ e1'                   *)
         (* ------------------------------- (9.3b) *)
         (*     ap(e1; e2) ↦ ap(e1'; e2)           *)
         STEP(Term.$$(TermOps.App, [e1', e2]))
  and succ (e) : d =
      case trystep e of
           (*             e val                      *)
           (* ------------------------------- (9.2b) *)
           (*            s(e) val                    *)
           VAL => VAL
           (*             e ↦ e'                     *)
           (* ------------------------------- (9.3a) *)
           (*          s(e) ↦ s(e')                  *)
        |  STEP(e') => STEP(Term.$$(TermOps.Succ, [e']))
  and recurse e0 xye1 e =
      case trystep e of
           (*                    e ↦ e'                            *)
           (* --------------------------------------------- (9.3e) *)
           (*    rec(e0; x.y.e1)(e) ↦ rec(e0; x.y.e1)(e')          *)
           STEP(e') => STEP(makeRec e0 xye1 e')
         | VAL =>
           case Term.out e of
               (* ------------------------------- (9.3f) *)
               (*     rec(e0; x.y.e1)(z) ↦ z             *)
               Term.$(TermOps.Zero, []) => STEP(e0)
               (*                        s(e) val                               *)
               (* ------------------------------------------------------ (9.3g) *)
               (* rec(e0; x.y.e1)(s(e)) ↦ [e, rec(e0; x.y.e1)(e)/x, y]e1        *)
             | Term.$(TermOps.Succ, [e']) =>
               (case Term.out xye1 of
                    Term.\ (x, ye1) =>
                    (case Term.out ye1 of
                         Term.\ (y, e1) =>
                         let
                             val e1'  = Term.subst e' x e1
                             val e1'' = Term.subst (makeRec e0 xye1 e') y e1'
                         in
                             STEP(e1'')
                         end
                       | _ => raise Malformed)
                  | _ => raise Malformed)
             | _ => raise Malformed

  fun eval e =
    case trystep e of
        STEP e' => (print ("Step: " ^ Term.toString e' ^ "\n"); eval e')
      | VAL => e
end
