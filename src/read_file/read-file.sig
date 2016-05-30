signature FILE_READER =
sig
  val evalFile : string -> Term.t
  val processFile : string -> unit
    val main : 'a * string list -> int
end
