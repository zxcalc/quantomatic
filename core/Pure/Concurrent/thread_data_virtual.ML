(*  Title:      Pure/Concurrent/thread_data_virtual.ML
    Author:     Makarius

Thread-local data -- virtual version with context management.
*)

structure Thread_Data_Virtual: THREAD_DATA =
struct

structure Data = Generic_Data
(
  type T = Universal.universal Inttab.table;
  val empty = Inttab.empty;
  val extend = I;
  val merge = Inttab.merge (K true);
);

abstype 'a var = Var of serial * 'a Universal.tag
with

fun var () : 'a var = Var (serial (), Universal.tag ());

fun get (Var (i, tag)) =
  Inttab.lookup (Data.get (Context.the_generic_context ())) i
  |> Option.map (Universal.tagProject tag);

fun put (Var (i, tag)) data =
  (Context.>> o Data.map)
    (case data of
      NONE => Inttab.delete_safe i
    | SOME x => Inttab.update (i, Universal.tagInject tag x));

fun setmp v data f x =
  Thread_Attributes.uninterruptible (fn restore_attributes => fn () =>
    let
      val orig_data = get v;
      val _ = put v data;
      val result = Exn.capture (restore_attributes f) x;
      val _ = put v orig_data;
    in Exn.release result end) ();

end;

val is_virtual = true;

end;
