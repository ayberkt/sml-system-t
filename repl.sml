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

  fun parse () =
    let val lexer = ExpParser.makeLexer
                        (fn _ => valOf (TextIO.inputLine TextIO.stdIn))
        val dummyEOF = ExpLrVals.Tokens.EOF(0,0)
        val dummySEMI = ExpLrVals.Tokens.SEMI(0,0)
        fun loop lexer =
          let val (result, lexer) = invoke lexer
              val (nextToken, lexer) = ExpParser.Stream.get lexer
          in (TextIO.output(TextIO.stdOut,
                            (">>> " ^ (Term.toString result) ^ " : "
                             ^ Type.toString (TypeChecker.typecheck Context.empty result) ^ "\n"));
              if ExpParser.sameToken(nextToken,dummyEOF)
              then ()
              else loop lexer)
          end
    in loop lexer
    end

    fun main _ = (parse (); 1)

end
