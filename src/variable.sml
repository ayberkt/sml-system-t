structure Var : VARIABLE =
struct
  type t = string * int

  val counter = ref 0

  fun newvar s =
    let
        val (ref n) = counter
    in
        (counter := n + 1 ; (s, n))
    end

  fun compare ((str, n), (str', m)) =
    (case Int.compare (n, m) of
        EQUAL => String.compare (str, str')
      | order => order)

  fun equal (x, y) = compare (x, y) = EQUAL

  fun toString (s, x) = s

end
