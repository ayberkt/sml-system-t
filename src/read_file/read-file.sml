structure FileReader : FILE_READER =
struct

  exception NotImplemented

  fun hdl f input =
  SOME (f input)
  handle ParserState.Parse(s) =>
         ((TextIO.print (s ^ "\n")); NONE)
       | (TypeChecker.TypeError msg) =>
         ((TextIO.print ("Type error: " ^ msg ^ ".\n")); NONE)
       | Dynamics.Malformed =>
         ((TextIO.print "Something went seriously wrong in UncheckedDynamics!\n") ; NONE)
       | _ => (TextIO.print "Unknown error\n"; NONE)

  fun readFile (file : string) : string =
  let
    val ins = TextIO.openIn file
  in
    TextIO.input ins
  end


  fun evalFile file =
  let
    val input = readFile file
  in
    case hdl (Dynamics.eval o Parser.parse) input of
      SOME term => term
  end

  fun processFile file =
  let
    fun printLn s = print (s ^ "\n")
  in
    (printLn o Term.toString) (evalFile file)
  end
end
