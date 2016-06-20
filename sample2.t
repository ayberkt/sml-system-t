let
  val plus  =
    \(n:nat) \(m:nat) rec m {
      z           => n
    | s(x) with y => s(y)
    }
in
  let
      val times =
        \(n:nat) \(m:nat) rec m {
          z           => z
        | s(x) with y => plus(y)(n)
        }
  in
    let
      val six = s(s(s(s(s(s(z))))))
    in
      times(six)(six)
    end
  end
end
