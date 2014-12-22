(*  Title:      Pure/Tools/find_consts.ML
    Author:     Timothy Bourke and Gerwin Klein, NICTA

Hoogle-like (http://www-users.cs.york.ac.uk/~ndm/hoogle) searching by
type over constants, but matching is not fuzzy.
*)

signature FIND_CONSTS =
sig
  datatype criterion =
      Strict of string
    | Loose of string
    | Name of string
  val read_query: Position.T -> string -> (bool * criterion) list
  val find_consts : Proof.context -> (bool * criterion) list -> unit
end;

structure Find_Consts : FIND_CONSTS =
struct

(* search criteria *)

datatype criterion =
    Strict of string
  | Loose of string
  | Name of string;


(* matching types/consts *)

fun matches_subtype thy typat =
  Term.exists_subtype (fn ty => Sign.typ_instance thy (ty, typat));

fun check_const pred (nm, (ty, _)) =
  if pred (nm, ty) then SOME (Term.size_of_typ ty) else NONE;

fun opt_not f (c as (_, (ty, _))) =
  if is_some (f c) then NONE else SOME (Term.size_of_typ ty);

fun filter_const _ _ NONE = NONE
  | filter_const c f (SOME rank) =
      (case f c of
        NONE => NONE
      | SOME i => SOME (Int.min (rank, i)));


(* pretty results *)

fun pretty_criterion (b, c) =
  let
    fun prfx s = if b then s else "-" ^ s;
  in
    (case c of
      Strict pat => Pretty.str (prfx "strict: " ^ quote pat)
    | Loose pat => Pretty.str (prfx (quote pat))
    | Name name => Pretty.str (prfx "name: " ^ quote name))
  end;

fun pretty_const ctxt (c, ty) =
  let
    val ty' = Logic.unvarifyT_global ty;
    val const_space = Consts.space_of (Sign.consts_of (Proof_Context.theory_of ctxt));
    val markup = Name_Space.markup const_space c;
  in
    Pretty.block
     [Pretty.mark markup (Pretty.str c), Pretty.str " ::", Pretty.brk 1,
      Pretty.quote (Syntax.pretty_typ ctxt ty')]
  end;


(* find_consts *)

fun pretty_consts ctxt raw_criteria =
  let
    val thy = Proof_Context.theory_of ctxt;
    val low_ranking = 10000;

    fun user_visible consts (nm, _) =
      if Consts.is_concealed consts nm then NONE else SOME low_ranking;

    fun make_pattern crit =
      let
        val raw_T = Syntax.parse_typ ctxt crit;
        val t =
          Syntax.check_term
            (Proof_Context.set_mode Proof_Context.mode_pattern ctxt)
            (Term.dummy_pattern raw_T);
      in Term.type_of t end;

    fun make_match (Strict arg) =
          let val qty = make_pattern arg; in
            fn (_, (ty, _)) =>
              let
                val tye = Sign.typ_match thy (qty, ty) Vartab.empty;
                val sub_size =
                  Vartab.fold (fn (_, (_, t)) => fn n => Term.size_of_typ t + n) tye 0;
              in SOME sub_size end handle Type.TYPE_MATCH => NONE
          end
      | make_match (Loose arg) =
          check_const (matches_subtype thy (make_pattern arg) o snd)
      | make_match (Name arg) = check_const (match_string arg o fst);

    fun make_criterion (b, crit) = (if b then I else opt_not) (make_match crit);
    val criteria = map make_criterion raw_criteria;

    val consts = Sign.consts_of thy;
    val {constants, ...} = Consts.dest consts;
    fun eval_entry c =
      fold (filter_const c) (user_visible consts :: criteria) (SOME low_ranking);

    val matches =
      fold (fn c => (case eval_entry c of NONE => I | SOME rank => cons (rank, c))) constants []
      |> sort (prod_ord (rev_order o int_ord) (string_ord o pairself fst))
      |> map (apsnd fst o snd);

    val position_markup = Position.markup (Position.thread_data ()) Markup.position;
  in
    Pretty.block
      (Pretty.fbreaks
        (Pretty.mark position_markup (Pretty.keyword1 "find_consts") ::
          map pretty_criterion raw_criteria)) ::
    Pretty.str "" ::
    (if null matches then [Pretty.str "found nothing"]
     else
       Pretty.str ("found " ^ string_of_int (length matches) ^ " constant(s):") ::
       grouped 10 Par_List.map (Pretty.item o single o pretty_const ctxt) matches)
  end |> Pretty.fbreaks |> curry Pretty.blk 0;

fun find_consts ctxt args = Pretty.writeln (pretty_consts ctxt args);


(* command syntax *)

local

val criterion =
  Parse.reserved "strict" |-- Parse.!!! (Parse.$$$ ":" |-- Parse.xname) >> Strict ||
  Parse.reserved "name" |-- Parse.!!! (Parse.$$$ ":" |-- Parse.xname) >> Name ||
  Parse.xname >> Loose;

val query = Scan.repeat ((Scan.option Parse.minus >> is_none) -- criterion);

in

fun read_query pos str =
  Outer_Syntax.scan pos str
  |> filter Token.is_proper
  |> Scan.error (Scan.finite Token.stopper (Parse.!!! (query --| Scan.ahead Parse.eof)))
  |> #1;

val _ =
  Outer_Syntax.improper_command @{command_spec "find_consts"}
    "find constants by name / type patterns"
    (query >> (fn spec =>
      Toplevel.keep (fn st =>
        Pretty.writeln (pretty_consts (Toplevel.context_of st) spec))));

end;


(* PIDE query operation *)

val _ =
  Query_Operation.register "find_consts" (fn {state, args, output_result} =>
    (case try Toplevel.context_of state of
      SOME ctxt =>
        let
          val [query_arg] = args;
          val query = read_query Position.none query_arg;
        in output_result (Pretty.string_of (pretty_consts ctxt query)) end
    | NONE => error "Unknown context"));

end;

