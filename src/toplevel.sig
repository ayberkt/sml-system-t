signature TOPLEVEL = 
sig

val eval : string -> TopLevelCommands.res
val repl : unit -> unit
(*val parse_vars : string list * string -> Term.t*)

end
