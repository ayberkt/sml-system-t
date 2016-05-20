structure List_Util : LIST_UTIL =
struct

  open List

  fun zip_exact e []        []        = []
    | zip_exact e (x :: xs) (y :: ys) =
        (x, y) :: zip_exact e xs ys
    | zip_exact e _ _ = raise e

  fun remove eq x xs = List.filter (fn y => not(eq(x,y))) xs

  fun nub _ [] = []
    | nub eq (x :: xs) = x :: nub eq (remove eq x xs)

  fun collate eq xss =
    nub eq (List.concat xss)

  fun zipTest _ [] _  = true
    | zipTest _ _  [] = true
    | zipTest p (x :: xs) (y :: ys) = p (x, y) andalso zipTest p xs ys

end
