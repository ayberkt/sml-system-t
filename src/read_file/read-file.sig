signature FILE_READER =
sig
  val evalFile : string -> Term.t
  val processFile : string -> unit
end
