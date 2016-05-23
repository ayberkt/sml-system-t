structure TopLevel : TOPLEVEL =
struct

structure TM = Term
structure TLC = TopLevelCommands
structure T = TypeChecker
structure CD = Dynamics
structure UD = Dynamics

exception InternalError
exception NothingToDo


fun checkedStep term =
    (case CD.view(CD.trystep term) of
          CD.Step (t) => (TLC.Next (t), SOME (TLC.Checked, t))
        | CD.Val      => (TLC.Val(term), NONE)
        | CD.Err      => (TLC.Err, NONE)
    )

fun uncheckedStep term =
    (case UD.view(UD.trystep term) of
          UD.Step (t) => (TLC.Next (t), SOME (TLC.Unchecked, t))
        | UD.Val      => (TLC.Val (term), NONE)
        | UD.Err      => raise InternalError
    )

fun checkType term =
    let
        val tau = T.typecheck Context.empty term
        val _ = TextIO.print ("Statics : exp has type : " ^ (Type.toString tau) ^ "\n");
    in
      tau
    end


fun doStep stepmem x =
    let
        val (mode, term) =
            (case x of
                  SOME(t) => t
                | NONE => case stepmem of
                              SOME(t) => t
                            | NONE => raise NothingToDo)
        val _ = if(isSome x andalso (mode = TLC.Unchecked))
                then checkType term else Type.$$ (TypeOps.NAT, [])
    in
            case mode of
                  TLC.Unchecked => uncheckedStep term
                | TLC.Checked   => checkedStep term
    end

fun doEval stepmem x =
    let
        val (mode, term) =
            (case x of
                  SOME(t) => t
                | NONE => case stepmem of
                              SOME(t) => t
                            | NONE => raise NothingToDo)
        val _ = if(isSome x andalso mode = TLC.Unchecked)
                then checkType term else Type.$$ (TypeOps.NAT, [])
    in
            case mode of
                  TLC.Checked => (TLC.Val (CD.eval term), NONE)
                | TLC.Unchecked   => (TLC.Val (UD.eval term), NONE)
    end


(* evalCore : (dynMode * Term.t) option -> TLC.cmd -> (TLC.res * (dynMode * Term.t) option)*)
fun evalCore stepmem parseRes =
     (case parseRes of
         TLC.Step(x) => doStep stepmem x
       | TLC.Eval(x) => doEval stepmem x
     )
(*
fun resToString result =
      let
         val TLC.Result(var, res, res_type) = result
      in
         ("val " ^ (Var.toUserString var) ^ " = " ^ (Real.toString res)
            ^ " : " ^ (TypeChecker.toString res_type) ^ "\n" )
      end
*)
fun resToString result =
    (case result of
         TLC.Next(e) => " --> " ^Term.toString(e) ^"\n\n"
       | TLC.Val(e)  => " " ^Term.toString(e) ^" VAL\n\n"
       | TLC.Err     => " ERR\n\n"
    )

fun processDef text (res,stepmem) =
   let
      val parseRes = Parse.parse symbols.empty text
      val (res, stepmem') = evalCore stepmem parseRes
	  val output = resToString res
   in
   	  (SOME(res),stepmem')
   end

fun processDefRepl text a =
   let
      val (res,stepmem') = processDef text a
   in
      case res of
         SOME(r) => (TextIO.print (resToString r) ; (res,stepmem'))
      |  NONE => raise InternalError
   end

fun hdl f x y = (f x y)
      handle ParserState.Parse(s) =>
	  		((TextIO.print ("Parse error: " ^ s ^ "\n")) ; y)
	   | T.TypeError _ => ((TextIO.print "TypeChecker error!\n") ; y)
	   | CD.RuntimeError => ((TextIO.print "Could not evaluate!\n") ; y)
	   | CD.Malformed => ((TextIO.print "Something went seriously wrong in CheckedDynamics!\n") ; y)
	   | UD.Malformed => ((TextIO.print "Something went seriously wrong in UncheckedDynamics!\n") ; y)
	   | NothingToDo => ((TextIO.print "Nothing to do!\n") ; y)
       |  _ => ((TextIO.print "Error!\n"); y)

local
   fun revtostring L = String.implode (foldl (op ::) [] L)
in
fun stringStream L s = case Stream.force s of
      (Stream.Cons((#";",_),s)) =>
	     Stream.cons(revtostring L,
		 Stream.delay(fn () => Stream.force(stringStream [] s)))
    | (Stream.Cons((c,_),s)) => stringStream (c::L) s
    | (Stream.Nil) => Stream.cons(revtostring L,Stream.empty)
end

fun repl () =
   (Stream.fold (hdl processDefRepl) (NONE,NONE)
      (Input.promptKeybd "->" "=>" (stringStream [])) ; ())

fun eval s =
   let
      val (res,_) = Stream.fold (processDef) (NONE,NONE)
         (stringStream []
      	   (foldr (Stream.cons) (Stream.empty)
      	   (map (fn c => (c,[])) (String.explode s))))
   in
      case res of
         SOME(r) => r
      |  NONE => ( print "No result!\n" ; raise InternalError )
   end

(*fun parse_vars (vars,text) =
	(case (Parse.parse
		(foldr (fn (x,vs) =>
			symbols.insert(vs,x,Var.newvar x))
			(EN.symbolTable (EN.newenv())) vars)
		text) of
		TLC.Def(_,_,t) => t
	|	TLC.Eval(t) => t)
*)
end


