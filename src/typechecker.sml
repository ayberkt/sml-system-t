structure TypeChecker : TYPECHECKER =
struct

    exception TypeError of string

    type context = Type.t Context.map

    fun equiv t1 t2 = Type.aequiv (t1, t2)

    fun typecheck ctx e =
      let
          val nat : Type.t = Type.$$((TypeOps.NAT), [] : Type.t list)
      in
          (case Term.out e of
              (* ------------------------------- *)
              (*          Γ ⊢ Zero : nat         *)
              Term.$(TermOps.Zero, _) => nat

              (*           Γ ⊢ x : nat           *)
              (* ------------------------------- *)
              (*         Γ ⊢ (Succ x) : nat      *)
            | Term.$(TermOps.Succ, [x]) =>
              if (equiv (typecheck ctx x) nat)
              then nat
              else raise TypeError "s can be only applied to a nat."

            | Term.$((TermOps.Lam t1), [e]) =>
              (*       𝚪, var:t1 ⊢ body:t2          *)
              (* ---------------------------------- *)
              (*   Γ ⊢ lam{t1}(var.body) : t1 → t2  *)
              let
                  val (Term.\ (var, body)) = Term.out e
                  val ctx' = Context.insert(ctx, var, t1)
                  val t2 : Type.t = typecheck ctx' body
                  val t1tot2 : Type.t = Type.$$(TypeOps.ARR, [nat, nat])
              in
                  t1tot2
              end
            | Term.$(TermOps.App, [f, x]) =>
              (*   𝚪, f:arr(t1, t2)      Γ ⊢ x:t1   *)
              (* ---------------------------------- *)
              (*          Γ ⊢ ap(f, x) : t2         *)
              let
                  val [t1, t2] =
                      case Type.out (typecheck ctx f) of
                          Type.$(TypeOps.ARR, [t1, t2]) => [t1, t2]
                        | _ => raise TypeError "Operator must have arrow type."
                  val xTy = typecheck ctx x
              in
                  if equiv xTy t1
                  then t2
                  else raise TypeError "Operand type does not match the operator domain."
              end
            | Term.$(TermOps.Rec, [e0, e1, e]) =>
              (let
                  val eTy = typecheck ctx e
                  val e0Ty : Type.t = typecheck ctx e0
                  val e1Ty =
                      case Term.out e1 of
                          Term.\ (x, body') =>
                          case Term.out body' of
                              Term.\ (y, body) =>
                              let
                                  val ctx'  = Context.insert(ctx, x, nat)
                                  val ctx'' = Context.insert(ctx', y, e0Ty)
                              in
                                  typecheck ctx'' body
                              end
              in
                  if equiv e1Ty e0Ty andalso equiv eTy nat
                  then e1Ty
                  else raise TypeError (Type.toString e1Ty ^ "not equal to" ^ Type.toString e0Ty)
              end)
            | Term.`(x) =>
              (* ------------------------------- *)
              (*        Γ, x : t ⊢ x : t         *)
              (case Context.find(ctx, x) of
                  SOME t => t
               |  NONE => raise TypeError ("variable " ^ (Var.toString x) ^ " cannot be found"))
            | _ => raise TypeError "no rule applies")
      end
end
