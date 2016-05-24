signature VARIABLE =
sig
  type t

  (* Creates a new, globally unique variable. *)
  val newvar : string -> t

  (* Tests whether two variable are equal. *)
  val equal : t * t -> bool

  (* Compares two variables.
     This is used to allow variables as keys
     into a hash table. *)
  val compare : t * t -> order

  (* Provides a string representation of the globally unique
     variable. *)
  val toString : t -> string

  (* Provides the string used to create the variable. *)
  val toUserString : t -> string

end

