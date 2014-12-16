(*  Title:      Pure/General/linear_set.ML
    Author:     Makarius

Sets with canonical linear order, or immutable linked-lists.
*)

signature LINEAR_SET =
sig
  type key
  type 'a T
  exception DUPLICATE of key
  exception UNDEFINED of key
  exception NEXT_UNDEFINED of key option
  val empty: 'a T
  val is_empty: 'a T -> bool
  val defined: 'a T -> key -> bool
  val lookup: 'a T -> key -> ('a * key option) option
  val update: key * 'a -> 'a T -> 'a T
  val iterate: key option ->
    ((key option * key) * 'a -> 'b -> 'b option) -> 'a T -> 'b -> 'b
  val dest: 'a T -> (key * 'a) list
  val get_after: 'a T -> key option -> key option
  val insert_after: key option -> key * 'a -> 'a T -> 'a T
  val delete_after: key option -> 'a T -> 'a T
end;

functor Linear_Set(Key: KEY): LINEAR_SET =
struct

(* type key *)

type key = Key.key;
structure Table = Table(Key);

exception DUPLICATE of key;
exception UNDEFINED of key;
exception NEXT_UNDEFINED of key option;


(* raw entries *)

fun the_entry entries key =
  (case Table.lookup entries key of
    NONE => raise UNDEFINED key
  | SOME entry => entry);

fun next_entry entries key = snd (the_entry entries key);

fun put_entry entry entries = Table.update entry entries;

fun new_entry entry entries = Table.update_new entry entries
  handle Table.DUP key => raise DUPLICATE key;

fun del_entry key entries = Table.delete_safe key entries;


(* set representation and basic operations *)

datatype 'a T = Set of {start: key option, entries: ('a * key option) Table.table};

fun make_set (start, entries) = Set {start = start, entries = entries};
fun map_set f (Set {start, entries}) = make_set (f (start, entries));

fun start_of (Set {start, ...}) = start;
fun entries_of (Set {entries, ...}) = entries;

val empty = Set {start = NONE, entries = Table.empty};
fun is_empty set = is_none (start_of set);

fun defined set key = Table.defined (entries_of set) key;

fun lookup set key = Table.lookup (entries_of set) key;

fun update (key, x) = map_set (fn (start, entries) =>
  (start, put_entry (key, (x, next_entry entries key)) entries));


(* iterate entries *)

fun optional_start set NONE = start_of set
  | optional_start _ some = some;

fun iterate opt_start f set =
  let
    val entries = entries_of set;
    fun apply _ NONE y = y
      | apply prev (SOME key) y =
          let
            val (x, next) = the_entry entries key;
            val item = ((prev, key), x);
          in
            (case f item y of
              NONE => y
            | SOME y' => apply (SOME key) next y')
          end;
  in apply NONE (optional_start set opt_start) end;

fun dest set = rev (iterate NONE (fn ((_, key), x) => SOME o cons (key, x)) set []);


(* relative addressing *)

fun get_after set hook =
  (case hook of
    NONE => start_of set
  | SOME key => next_entry (entries_of set) key);

fun insert_after hook (key, x) = map_set (fn (start, entries) =>
  (case hook of
    NONE => (SOME key, new_entry (key, (x, start)) entries)
  | SOME key1 =>
      let
        val (x1, next) = the_entry entries key1;
        val entries' = entries
          |> put_entry (key1, (x1, SOME key))
          |> new_entry (key, (x, next));
      in (start, entries') end));

fun delete_after hook set = set |> map_set (fn (start, entries) =>
  (case hook of
    NONE =>
      (case start of
        NONE => raise NEXT_UNDEFINED NONE
      | SOME key1 => (next_entry entries key1, del_entry key1 entries))
  | SOME key1 =>
      (case the_entry entries key1 of
        (_, NONE) => raise NEXT_UNDEFINED (SOME key1)
      | (x1, SOME key2) =>
          let
            val entries' = entries
              |> put_entry (key1, (x1, next_entry entries key2))
              |> del_entry key2;
          in (start, entries') end)));


(* ML pretty-printing *)

val _ =
  PolyML.addPrettyPrinter (fn depth => fn pretty => fn set =>
    ml_pretty
      (ML_Pretty.enum "," "{" "}"
        (ML_Pretty.pair (pretty_ml o PolyML.prettyRepresentation) (pretty_ml o pretty))
        (dest set, depth)));

end;

