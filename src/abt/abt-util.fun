functor ABT_Util(A : ABT) : ABT_UTIL =
struct
  open A
  open List_Util

  fun freevars e =
    case A.out e of
      (* freevars of a var view consists only of itself *)
        (A.` x) => [x]
      (* freevars of f applied to es, are the unique freevars
      in all of e1, e2,..., en. *)
      | (A.$ (f, es)) =>
          List_Util.collate A.Variable.equal (List.map freevars es)
      (* Free vars of an abstraction view are the ones in the body,
         except the variable `z` which is the one being abstracted.*)
      | (A.\ (z, e')) => List_Util.remove A.Variable.equal z (freevars e')

  val `` = A.into o A.`
  val \\ = A.into o A.\
  val $$ = A.into o A.$


  (* Substitute `e` for `x` in `body`. *)
  fun subst e x body =
    let
        val body' =
            case A.out body of
                A.` y => if A.Variable.equal(x, y)
                         then A.out e
                         else A.` y
             | A.$(f, args) => A.$ (f, List.map (subst e x) args)
             | A.\ (z, arg) => A.\ (z, subst e x arg)
    in
        A.into body'
    end

  fun toString e =
    case A.out e of
        A.` x => A.Variable.toString x
      | A.$ (f, es) =>
        let
            val esStr =
                if (null es) then "" else "(" ^ (toStrings es) ^ ")"
        in
            A.Operator.toString f ^ esStr
        end
     | A.\ (x, e) => (A.Variable.toString x) ^ "." ^ (toString e)
  and toStrings [] = ""
    | toStrings [e] = toString e
    | toStrings (e :: es) = (toString e) ^ "; " ^ (toStrings es)

end
