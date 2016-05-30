(\ (x:nat) rec x {
    z => z
  | s(u) with v => s(s(v))
})(s(s(z)))
