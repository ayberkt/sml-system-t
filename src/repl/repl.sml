structure Repl =
struct

  fun hdl f input =
    SOME (f input)
    handle ParserState.Parse(s) =>
	  		   ((TextIO.print (s ^ "\n")); NONE)
	       | (TypeChecker.TypeError msg) =>
           ((TextIO.print ("Type error: " ^ msg ^ ".\n")); NONE)
	       | Dynamics.Malformed =>
           ((TextIO.print "Something went seriously wrong in UncheckedDynamics!\n") ; NONE)
         | _ => (TextIO.print "Unknown error\n"; NONE)

  fun loop f =
    let val dummyEOF = ExpLrVals.Tokens.EOF(0, 0)
        val input = valOf ( TextIO.output(TextIO.stdOut, "> ")
                          ; TextIO.flushOut(TextIO.stdOut)
                          ; TextIO.inputLine TextIO.stdIn)
        val result = f input
    in
        case result of
             SOME term =>
             (case hdl (TypeChecker.typecheck Context.empty) term of
                 SOME tau => print ((Term.toString term) ^ " : " ^ (Type.toString tau) ^ "\n")
               | NONE => (); loop f)
          | NONE => raise (Fail "unknown error")
    end


  fun parseLoop () = (
      Parser.init symbols.empty;
      loop (hdl Parser.parse)
  )

  fun evalLoop () = (
      Parser.init symbols.empty;
      loop (hdl (Dynamics.eval o Parser.parse))
  )

  fun main _ = (evalLoop (); 1)

end
