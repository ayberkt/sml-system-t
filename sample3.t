let val plus = \(n:nat) \(m:nat)
  rec m {
      z           => n
  | s(x) with y => s(y)
  } in
let val times = \(n:nat) \(m:nat)
  rec m {
    z           => z
  | s(x) with y => plus(y)(n)
  } in
let
  val five  = s(s(s(s(s(z)))))
  val three = s(s(s(z)))
  val two   = s(s(z))
in
  plus(times(three)(five))(times(two)(plus(two)(two)))
end
end
end
