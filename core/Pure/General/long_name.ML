(*  Title:      Pure/General/long_name.ML
    Author:     Makarius

Long names.
*)

signature LONG_NAME =
sig
  val separator: string
  val is_qualified: string -> bool
  val hidden: string -> string
  val is_hidden: string -> bool
  val dest_hidden: string -> string option
  val localN: string
  val dest_local: string -> string option
  val implode: string list -> string
  val explode: string -> string list
  val append: string -> string -> string
  val qualification: string -> int
  val qualify: string -> string -> string
  val qualifier: string -> string
  val base_name: string -> string
  val map_base_name: (string -> string) -> string -> string
end;

structure Long_Name: LONG_NAME =
struct

val separator = ".";

val is_qualified = exists_string (fn s => s = separator);

val hidden_prefix = "??."
val hidden = prefix hidden_prefix;
val is_hidden = String.isPrefix hidden_prefix;
val dest_hidden = try (unprefix hidden_prefix);

val localN = "local";
val dest_local = try (unprefix "local.");

val implode = space_implode separator;
val explode = space_explode separator;

fun append name1 "" = name1
  | append "" name2 = name2
  | append name1 name2 = name1 ^ separator ^ name2;

fun qualification "" = 0
  | qualification name = fold_string (fn s => s = separator ? Integer.add 1) name 1;

fun qualify qual name =
  if qual = "" orelse name = "" then name
  else qual ^ separator ^ name;

fun qualifier "" = ""
  | qualifier name = implode (#1 (split_last (explode name)));

fun base_name "" = ""
  | base_name name = List.last (explode name);

fun map_base_name _ "" = ""
  | map_base_name f name =
      let val names = explode name
      in implode (nth_map (length names - 1) f names) end;

end;

