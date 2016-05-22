structure TypeChecker : TYPECHECKER =
struct

    exception TypeError

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
              else raise TypeError

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
              let
                  val (Type.$(TypeOps.ARR, [t1 : Type.t, t2])) =
                      Type.out (typecheck ctx f)
                  val xTy = typecheck ctx x
              in
                  if equiv xTy t1
                  then t2
                  else raise TypeError
              end
          )
      end
end
