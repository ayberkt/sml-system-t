signature TYPECHECKER =
sig
    exception TypeError

    type context = Type.t Context.map

    val typecheck : context -> Term.t -> Type.t
end
