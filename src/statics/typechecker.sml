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
               (* ------------------------------- (9.1a) *)
               (*        𝚪, x : t ⊢ x : t                *)
               Term.`(x) =>
               (case Context.find(ctx, x) of
                    SOME t => t
                 |  NONE => raise TypeError ("variable " ^ (Var.toString x) ^ " cannot be found"))

              (* ------------------------------- (9.1b) *)
              (*          𝚪 ⊢ Zero : nat                *)
            | Term.$(TermOps.Zero, _) => nat

              (*           𝚪 ⊢ x : nat                  *)
              (* ------------------------------- (9.1c) *)
              (*         𝚪 ⊢ (Succ x) : nat             *)
            | Term.$(TermOps.Succ, [x]) =>
              if (equiv (typecheck ctx x) nat)
              then nat
              else raise TypeError "s can be only applied to a nat."

              (*  𝚪 ⊢ e : nat  𝚪 ⊢ e0 : t  𝚪, x : nat, y : t ⊢ e1 : t        *)
              (* ---------------------------------------------------- (9.1d) *)
              (*                  𝚪 ⊢ (Succ x) : nat                         *)
            | Term.$(TermOps.Rec, [e0, e1, e]) =>
              (let
                  val eTy = typecheck ctx e
                  val t : Type.t = typecheck ctx e0
                  val e1Ty =
                      case Term.out e1 of
                          Term.\ (x, body') =>
                          case Term.out body' of
                              Term.\ (y, body) =>
                              let
                                  val ctx'  = Context.insert(ctx, x, nat)
                                  val ctx'' = Context.insert(ctx', y, t)
                              in
                                  typecheck ctx'' body
                              end
              in
                  if equiv e1Ty t andalso equiv eTy nat
                  then e1Ty
                  else
                      let
                          val message = (Type.toString e1Ty ^ "not equal to" ^ Type.toString t)
                      in
                          raise TypeError message
                      end
              end)

              (*       𝚪, var : t1 ⊢ body : t2                 *)
              (* -------------------------------------- (9.1e) *)
              (*   𝚪 ⊢ lam{t1}(var.body) : arr(t1, t2)         *)
            | Term.$((TermOps.Lam t1), [e]) =>
              let
                  val (Term.\ (var, body)) = Term.out e
                  val ctx' = Context.insert(ctx, var, t1)
                  val t2 : Type.t = typecheck ctx' body
              in
                  Type.$$(TypeOps.ARR, [nat, nat])
              end

              (*   𝚪, f:arr(t1, t2)      𝚪 ⊢ x:t1          *)
              (* ---------------------------------- (9.1f) *)
              (*          𝚪 ⊢ ap(f, x) : t2                *)
            | Term.$(TermOps.App, [f, x]) =>
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
            | _ => raise TypeError "no rule applies")
      end
end
