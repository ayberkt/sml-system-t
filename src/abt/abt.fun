functor Abt(O : OPERATOR) :> ABT where type Variable.t = Var.t
                                 where type Operator.t = O.t =
struct

  open List_Util

  structure Variable = Var
  structure Operator = O

  datatype 'a view =
           ` of Variable.t
         | \ of Variable.t * 'a
         | $ of Operator.t * 'a list


  datatype t =
    FV of Var.t
  | BV of int
  | ABS of t
  | OPER of Operator.t * t list

  exception Malformed
  exception NotImplemented

  fun map f (` x) = ` x
    | map f (\ (x, a)) = \ (x, f a)
    | map f ($ (h, vs)) = $(h, List.map f vs)

  fun valence_ok (n : int, e : t) : bool =
    case e of
        ABS e' => if n > 0 then valence_ok (n-1, e') else false
      | _ => if n = 0 then true else false

  exception AssertionFailureName

  fun abs x t =
    let fun bind' i t =
          case t of
              FV y => if Var.equal(x, y) then BV i else FV y
            | ABS t => ABS (bind' (i+1) t)
            | BV n => BV n
            | OPER(f, ts) => OPER(f, List.map (bind' i) ts)
    in
        ABS (bind' 0 t)
    end

  fun unabs x t =
    let
        fun unabs' i t =
          case t of
              BV j => if i = j then FV x else BV j
            | FV x => FV x
            | ABS t => ABS (unabs' (i+1) t)
            | OPER(f, ts) => OPER(f, List.map (unabs' i) ts)
    in
        unabs' 0 t
    end

  fun into (` x) = FV x
    | into (\ (x, t)) = abs x t
    | into ($ (f, es)) =
      if List.all valence_ok (zip_exact Malformed (O.arity f) es)
      then OPER(f, es)
      else raise Malformed

  fun out t =
    case t of
     (* If `out` is applied to a bound variable, something went wrong.
        Bound variables are not entitites by themselves and therefore they
        be represented as a view *)
       BV _ => raise AssertionFailureName
     | FV x => ` x
     (* An application f to is is mapped to AppView `$` *)
     | OPER(f, ts) => $ (f, ts)
     (* As we unpack the ABT we rename free variable to guarantee
        freshness. *)
     | ABS(t) =>
         let
             val x' = Var.newvar "x"
         in
             \ (x', unabs x' t)
         end

  fun aequiv (FV x, FV y) = Var.equal(x, y)
    | aequiv (BV n, BV m) = (n = m)
    | aequiv (ABS t, ABS t') = aequiv(t, t')
    | aequiv (OPER(f, ts), OPER(f', ts')) =
        O.equal(f, f') andalso List_Util.zipTest aequiv ts ts'
    | aequiv (_, _) = false
end
