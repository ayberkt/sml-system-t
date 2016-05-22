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
              Term.$(TermOps.Zero, _) => nat
            | Term.$(TermOps.Succ, [x]) =>
              (*            Γ ⊢ x : nat         *)
              (* ------------------------------ *)
              (*         Γ ⊢ (Succ x) : nat     *)
              if (equiv (typecheck ctx x) nat)
              then nat
              else raise TypeError
            | Term.$((TermOps.Lam t1), [e]) =>
              let
                  val (Term.\ (var, body)) = Term.out e
                  (*       𝚪, var:t1 ⊢ body:t2          *)
                  (* ---------------------------------- *)
                  (*   Γ ⊢ lam{t1}(var.body) : t1 → t2  *)
                  val ctx' = Context.insert(ctx, var, t1)
                  val t2 : Type.t = typecheck ctx' body
                  val t1tot2 : Type.t = Type.$$(TypeOps.ARR, [nat, nat])
              in
                  t1tot2
              end)
      end
end
