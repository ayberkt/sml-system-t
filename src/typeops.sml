structure TypeOps =
struct

    datatype t = NAT
               | ARR

    fun arity NAT = []
      | arity ARR = [0, 0]

    fun equal (x : t, y : t) = x = y

    fun toString NAT = "nat"
      | toString ARR = "arr"
end
