signature LIST_UTIL =
sig
    val zip_exact : exn -> 'a list -> 'b list -> ('a * 'b) list

    val remove : ('a * 'a -> bool) -> 'a -> 'a list -> 'a list

    val collate : ('a * 'a -> bool) -> 'a list list -> 'a list

    val zipTest : ('a * 'b -> bool) -> 'a list -> 'b list -> bool
end
