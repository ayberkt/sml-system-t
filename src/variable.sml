structure Var : VARIABLE =
struct
  type t = string * int

  val counter = ref 0

  fun newvar s = (s, (counter := !counter + 1; !counter))

  fun equal ((_, n), (_, m)) = n = m

  fun compare ((_, n), (_, m)) = Int.compare(n, m)

  fun toString (s, n) = s ^ "@" ^ (Int.toString n)

end
