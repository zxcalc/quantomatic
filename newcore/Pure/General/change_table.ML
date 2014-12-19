(*  Title:      Pure/General/change_table.ML
    Author:     Makarius

Generic tables with extra bookkeeping of changes relative to some
common base version, subject to implicit block structure.  Support for
efficient join/merge of big global tables with small local updates.
*)

signature CHANGE_TABLE =
sig
  structure Table: TABLE
  type key = Table.key
  exception DUP of key
  exception SAME
  type 'a T
  val table_of: 'a T -> 'a Table.table
  val empty: 'a T
  val is_empty: 'a T -> bool
  val change_base: bool -> 'a T -> 'a T
  val change_ignore: 'a T -> 'a T
  val join: (key -> 'a * 'a -> 'a) (*exception SAME*) -> 'a T * 'a T -> 'a T  (*exception DUP*)
  val merge: ('a * 'a -> bool) -> 'a T * 'a T -> 'a T  (*exception DUP*)
  val fold: (key * 'b -> 'a -> 'a) -> 'b T -> 'a -> 'a
  val dest: 'a T -> (key * 'a) list
  val lookup_key: 'a T -> key -> (key * 'a) option
  val lookup: 'a T -> key -> 'a option
  val defined: 'a T -> key -> bool
  val update: key * 'a -> 'a T -> 'a T
  val update_new: key * 'a -> 'a T -> 'a T  (*exception DUP*)
  val map_entry: key -> ('a -> 'a) -> 'a T -> 'a T
  val map_default: key * 'a -> ('a -> 'a) -> 'a T -> 'a T
  val delete_safe: key -> 'a T -> 'a T
end;

functor Change_Table(Key: KEY): CHANGE_TABLE =
struct

structure Table = Table(Key);
type key = Table.key;

exception SAME = Table.SAME;
exception DUP = Table.DUP;


(* optional change *)

datatype change =
  No_Change | Change of {base: serial, depth: int, changes: Table.set option};

fun make_change base depth changes =
  Change {base = base, depth = depth, changes = changes};

fun ignore_change (Change {base, depth, changes = SOME _}) =
      make_change base depth NONE
  | ignore_change change = change;

fun update_change key (Change {base, depth, changes = SOME ch}) =
      make_change base depth (SOME (Table.insert (K true) (key, ()) ch))
  | update_change _ change = change;

fun base_change true No_Change =
      make_change (serial ()) 0 (SOME Table.empty)
  | base_change true (Change {base, depth, changes}) =
      make_change base (depth + 1) changes
  | base_change false (Change {base, depth, changes}) =
      if depth = 0 then No_Change else make_change base (depth - 1) changes
  | base_change false No_Change = raise Fail "Unbalanced change structure";

fun cannot_merge () = raise Fail "Cannot merge: incompatible change structure";

fun merge_change (No_Change, No_Change) = NONE
  | merge_change (Change change1, Change change2) =
      let
        val {base = base1, depth = depth1, changes = changes1} = change1;
        val {base = base2, depth = depth2, changes = changes2} = change2;
        val _ = if base1 = base2 andalso depth1 = depth2 then () else cannot_merge ();
        val (swapped, ch2) =
          (case (changes1, changes2) of
            (_, SOME ch2) => (false, ch2)
          | (SOME ch1, _) => (true, ch1)
          | _ => cannot_merge ());
      in SOME (swapped, ch2, make_change base1 depth1 NONE) end
  | merge_change _ = cannot_merge ();


(* table with changes *)

datatype 'a T = Change_Table of {change: change, table: 'a Table.table};

fun table_of (Change_Table {table, ...}) = table;

val empty = Change_Table {change = No_Change, table = Table.empty};

fun is_empty (Change_Table {change, table}) =
  (case change of No_Change => Table.is_empty table | _ => false);

fun make_change_table (change, table) = Change_Table {change = change, table = table};
fun map_change_table f (Change_Table {change, table}) = make_change_table (f (change, table));

fun change_base begin = (map_change_table o apfst) (base_change begin);
fun change_ignore arg = (map_change_table o apfst) ignore_change arg;


(* join and merge *)

fun join f (arg1, arg2) =
  let
    val Change_Table {change = change1, table = table1} = arg1;
    val Change_Table {change = change2, table = table2} = arg2;
  in
    if pointer_eq (change1, change2) andalso pointer_eq (table1, table2) then arg1
    else if is_empty arg2 then arg1
    else if is_empty arg1 then arg2
    else
      (case merge_change (change1, change2) of
        NONE => make_change_table (No_Change, Table.join f (table1, table2))
      | SOME (swapped, ch2, change') =>
          let
            fun maybe_swap (x, y) = if swapped then (y, x) else (x, y);
            val (tab1, tab2) = maybe_swap (table1, table2);
            fun update key tab =
              (case Table.lookup tab2 key of
                NONE => tab
              | SOME y =>
                  (case Table.lookup tab key of
                    NONE => Table.update (key, y) tab
                  | SOME x =>
                      (case (SOME (f key (maybe_swap (x, y))) handle Table.SAME => NONE) of
                        NONE => tab
                      | SOME z => Table.update (key, z) tab)));
          in make_change_table (change', Table.fold (update o #1) ch2 tab1) end)
  end;

fun merge eq =
  join (fn key => fn xy => if eq xy then raise Table.SAME else raise Table.DUP key);


(* derived operations *)

fun fold f arg = Table.fold f (table_of arg);
fun dest arg = Table.dest (table_of arg);
fun lookup_key arg = Table.lookup_key (table_of arg);
fun lookup arg = Table.lookup (table_of arg);
fun defined arg = Table.defined (table_of arg);

fun change_table key f =
  map_change_table (fn (change, table) => (update_change key change, f table));

fun update (key, x)        = change_table key (Table.update (key, x));
fun update_new (key, x)    = change_table key (Table.update_new (key, x));
fun map_entry key f        = change_table key (Table.map_entry key f);
fun map_default (key, x) f = change_table key (Table.map_default (key, x) f);
fun delete_safe key        = change_table key (Table.delete_safe key);

end;

structure Change_Table = Change_Table(type key = string val ord = fast_string_ord);

