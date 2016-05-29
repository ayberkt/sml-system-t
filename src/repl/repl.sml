structure Repl =
struct

  fun hdl f input =
    SOME (f input)
    handle ParserState.Parse(s) =>
	  		   ((TextIO.print ("Parse error: " ^ s ^ "\n")); NONE)
	       | (TypeChecker.TypeError msg) =>
           ((TextIO.print msg); NONE)
	       | Dynamics.Malformed =>
           ((TextIO.print "Something went seriously wrong in UncheckedDynamics!\n") ; NONE)

  fun loop f =
    let val dummyEOF = ExpLrVals.Tokens.EOF(0, 0)
        val input = valOf ( TextIO.output(TextIO.stdOut, "> ")
                          ; TextIO.flushOut(TextIO.stdOut)
                          ; TextIO.inputLine TextIO.stdIn)
        val result = f input
    in
        (case result of
             SOME r => TextIO.output(TextIO.stdOut,
                                     ((Term.toString r) ^ " : "
                                      ^ Type.toString (TypeChecker.typecheck Context.empty r) ^ "\n"))
           | NONE => ();
         loop f)
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
