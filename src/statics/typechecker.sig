signature TYPECHECKER =
sig
    exception TypeError of string

    type context = Type.t Context.map

    val typecheck : context -> Term.t -> Type.t
end
