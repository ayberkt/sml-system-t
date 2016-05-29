structure ParserState =
struct

structure T = Term
exception Parse of string

val symtable : Var.t symbols.map ref = ref symbols.empty
val stack : Var.t symbols.map list ref = ref nil

fun settable table =
 (symtable := table;
  stack := nil;
  ())

fun savetable () = ((stack := !symtable :: !stack); ())

fun restoretable () = ((symtable := hd(!stack));(stack := tl (!stack)); ())

fun getvar id =
  case symbols.find(!symtable, id) of
      SOME (v) => v
    | _ => raise Parse("Undefined identifier: " ^ id)

fun addvar id =
   let
      val var = Var.newvar id
      (* val _ = TextIO.print ("addvar" ^ (Var.toString var) ^ "\n") *)
   in
     ( (symtable := symbols.insert(!symtable, id, var));
     var
     )
   end

fun createLam (var : Var.t) (t : Type.t) (body : Term.t) : Term.t
  = T.$$(TermOps.Lam t, [(T.\\ (var, body))])

end
