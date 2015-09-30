(*  Title:      Pure/Concurrent/par_exn.ML
    Author:     Makarius

Parallel exceptions as flattened results from acyclic graph of
evaluations.  Interrupt counts as neutral element.
*)

signature PAR_EXN =
sig
  val identify: Properties.entry list -> exn -> exn
  val the_serial: exn -> int
  val make: exn list -> exn
  val dest: exn -> exn list option
  val is_interrupted: 'a Exn.result list -> bool
  val release_all: 'a Exn.result list -> 'a list
  val release_first: 'a Exn.result list -> 'a list
end;

structure Par_Exn: PAR_EXN =
struct

(* identification via serial numbers -- NOT portable! *)

fun identify default_props exn =
  let
    val props = Exn_Properties.get exn;
    val update_serial =
      if Properties.defined props Markup.serialN then []
      else [(Markup.serialN, serial_string ())];
    val update_props = filter_out (Properties.defined props o #1) default_props;
  in Exn_Properties.update (update_serial @ update_props) exn end;

fun the_serial exn =
  Markup.parse_int (the (Properties.get (Exn_Properties.get exn) Markup.serialN));

val exn_ord = rev_order o int_ord o apply2 the_serial;


(* parallel exceptions *)

exception Par_Exn of exn list;
  (*non-empty list with unique identified elements sorted by exn_ord;
    no occurrences of Par_Exn or Exn.Interrupt*)

fun par_exns (Par_Exn exns) = exns
  | par_exns exn = if Exn.is_interrupt exn then [] else [identify [] exn];

fun make exns =
  let
    val exnss = map par_exns exns;
    val exns' = Ord_List.unions exn_ord exnss handle Option.Option => flat exnss;
  in if null exns' then Exn.Interrupt else Par_Exn exns' end;

fun dest (Par_Exn exns) = SOME exns
  | dest exn = if Exn.is_interrupt exn then SOME [] else NONE;


(* parallel results *)

fun is_interrupted results =
  exists (fn Exn.Exn _ => true | _ => false) results andalso
    Exn.is_interrupt (make (map_filter Exn.get_exn results));

fun release_all results =
  if forall (fn Exn.Res _ => true | _ => false) results
  then map Exn.release results
  else raise make (map_filter Exn.get_exn results);

fun plain_exn (Exn.Res _) = NONE
  | plain_exn (Exn.Exn (Par_Exn _)) = NONE
  | plain_exn (Exn.Exn exn) = if Exn.is_interrupt exn then NONE else SOME exn;

fun release_first results =
  (case get_first plain_exn results of
    NONE => release_all results
  | SOME exn => reraise exn);

end;

