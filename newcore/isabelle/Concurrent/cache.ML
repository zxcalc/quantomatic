(*  Title:      Pure/Concurrent/cache.ML
    Author:     Makarius

Concurrently cached values, with minimal locking time and singleton
evaluation due to lazy storage.
*)

signature CACHE =
sig
  val create: 'table -> ('table -> 'key -> 'value lazy option) ->
    ('key * 'value lazy -> 'table -> 'table) -> ('key -> 'value) -> 'key -> 'value
end;

structure Cache: CACHE =
struct

fun create empty lookup update f =
  let
    val cache = Synchronized.var "cache" empty;
    fun apply x =
      Synchronized.change_result cache
        (fn tab =>
          (case lookup tab x of
            SOME y => (y, tab)
          | NONE =>
              let val y = Lazy.lazy (fn () => f x)
              in (y, update (x, y) tab) end))
      |> Lazy.force;
  in apply end;

end;

