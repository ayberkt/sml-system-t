let val plus = \(n:nat) \(m:nat) rec m {
      z           => n
    | s(x) with y => s(y)
    }
in let val pred = \(n : nat) rec n {
      z           => z
    | s(x) with y => x
    }
in let val minus = \(n:nat) \(m:nat) rec m {
      z           => n
    | s(y) with r => pred(r)
    }
in let val lte = \(a:nat) rec a {
      z           => \(b:nat) s(z)
    | s(x) with f => \(b:nat) rec b {
        z           => z
      | s(y) with r => f(y)
      }
    }
in let val modAux =
    \(gas:nat) rec gas {
      z           => \(a:nat) \(b:nat) a
    | s(y) with f => \(a:nat) \(b:nat) rec lte(b)(a) {
        z           => a
      | s(j) with k => f (minus(a)(b)) (b)
      }
    }
in let val mod = \(a:nat) \(b:nat) modAux (plus (a) (b)) (a) (b)
in let val gcdAux = \(gas:nat) rec gas {
      z           => \(a:nat) \(b:nat) a
    | s(y) with r => \(a:nat) \(b:nat) rec b {
        z           => a
      | s(y) with p => r (b) (mod (a) (b))
      }
    }
in let val gcd = \(a:nat) \(b:nat) gcdAux (plus (a) (b)) (a) (b)
in gcd (s(s(s(s(s(s(s(s(s(s(s(s(z)))))))))))))
       (s(s(s(s(s(s(s(s(z)))))))))
end end end end end end end end
