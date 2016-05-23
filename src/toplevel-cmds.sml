structure TopLevelCommands =
struct
  datatype dynMode = Checked | Unchecked
  datatype cmd = (*Def of string * Type.t option * Term.t | TypeCheck Term.t |*) Step of (dynMode * Term.t) option | Eval of (dynMode * Term.t) option  
  datatype res = Next of Term.t | Val of Term.t | Err
end
