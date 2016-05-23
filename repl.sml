structure Repl =
struct

  open TextIO

  fun main _ =
    let
        val _ =
            let
                val input = TextIO.inputLine TextIO.stdIn
            in
                case input of
                    SOME s =>
                    let
                        val term : Term.t = Parse.parse symbols.empty s
                        val t = TypeChecker.typecheck Context.empty term
                    in
                        print (Term.toString term ^ " : " ^ Type.toString t)
                    end
                 |  NONE => print "You provided no input."
            end
    in
        1
    end
end
