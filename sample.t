let
  val f = \ (x:nat) rec x {
            z           => z
          | s(u) with v => s(s(s(v)))
          }
in
  f((s(s(z))))
end
