structure Repl =
struct

  fun invoke lexstream =
    let
        fun print_error (s,i:int,_) =
          TextIO.output(TextIO.stdOut,
                        "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

    in
        ExpParser.parse(0,lexstream,print_error,())
    end


  fun parse s =
    let
        val lexer = ExpParser.makeLexer (fn _ => s)
        fun parse' lexer =
          let
              val dummySEMI = ExpLrVals.Tokens.SEMI(0, 0)
              val (result : Term.t, lexer') = invoke lexer
              val (nextToken, lexer'') = ExpParser.Stream.get lexer'
          in
              if ExpParser.sameToken(nextToken, dummySEMI)
              then result
              else parse' lexer''
          end
    in
        parse' lexer
    end

  fun parseLoop () =
    let val dummyEOF = ExpLrVals.Tokens.EOF(0, 0)
        val input = valOf ( TextIO.output(TextIO.stdOut, "> ")
                          ; TextIO.flushOut(TextIO.stdOut)
                          ; TextIO.inputLine TextIO.stdIn)
        val result = SOME (parse input)
                     handle ParserState.Parse(s) =>
	  		                    ((TextIO.print ("Parse error: " ^ s ^ "\n")); NONE)
	                        | (TypeChecker.TypeError msg) =>
                            ((TextIO.print msg); NONE)
	                        | Dynamics.Malformed =>
                            ((TextIO.print "Something went seriously wrong in UncheckedDynamics!\n") ; NONE)

    in
        (case result of
            SOME r => TextIO.output(TextIO.stdOut,
                                     ((Term.toString r) ^ " : "
                                      ^ Type.toString (TypeChecker.typecheck Context.empty r) ^ "\n"))
          | NONE => ();
         parseLoop ())
    end

end
