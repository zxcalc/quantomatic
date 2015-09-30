(*  Title:      Pure/defs.ML
    Author:     Makarius

Global well-formedness checks for constant definitions.  Covers plain
definitions and simple sub-structural overloading.
*)

signature DEFS =
sig
  val pretty_const: Proof.context -> string * typ list -> Pretty.T
  val plain_args: typ list -> bool
  type T
  type spec =
   {def: string option,
    description: string,
    pos: Position.T,
    lhs: typ list,
    rhs: (string * typ list) list}
  val all_specifications_of: T -> (string * spec list) list
  val specifications_of: T -> string -> spec list
  val dest: T ->
   {restricts: ((string * typ list) * string) list,
    reducts: ((string * typ list) * (string * typ list) list) list}
  val empty: T
  val merge: Proof.context -> T * T -> T
  val define: Proof.context -> bool -> string option -> string ->
    string * typ list -> (string * typ list) list -> T -> T
end

structure Defs: DEFS =
struct

(* type arguments *)

type args = typ list;

fun pretty_const ctxt (c, args) =
  let
    val prt_args =
      if null args then []
      else [Pretty.list "(" ")" (map (Syntax.pretty_typ ctxt o Logic.unvarifyT_global) args)];
  in Pretty.block (Pretty.str c :: prt_args) end;

fun plain_args args =
  forall Term.is_TVar args andalso not (has_duplicates (op =) args);

fun disjoint_args (Ts, Us) =
  not (Type.could_unifys (Ts, Us)) orelse
    ((Type.raw_unifys (Ts, map (Logic.incr_tvar (maxidx_of_typs Ts + 1)) Us) Vartab.empty; false)
      handle Type.TUNIFY => true);

fun match_args (Ts, Us) =
  if Type.could_matches (Ts, Us) then
    Option.map Envir.subst_type
      (SOME (Type.raw_matches (Ts, Us) Vartab.empty) handle Type.TYPE_MATCH => NONE)
  else NONE;


(* datatype defs *)

type spec =
 {def: string option,
  description: string,
  pos: Position.T,
  lhs: args,
  rhs: (string * args) list};

type def =
 {specs: spec Inttab.table,  (*source specifications*)
  restricts: (args * string) list,  (*global restrictions imposed by incomplete patterns*)
  reducts: (args * (string * args) list) list};  (*specifications as reduction system*)

fun make_def (specs, restricts, reducts) =
  {specs = specs, restricts = restricts, reducts = reducts}: def;

fun map_def c f =
  Symtab.default (c, make_def (Inttab.empty, [], [])) #>
  Symtab.map_entry c (fn {specs, restricts, reducts}: def =>
    make_def (f (specs, restricts, reducts)));


datatype T = Defs of def Symtab.table;

fun lookup_list which defs c =
  (case Symtab.lookup defs c of
    SOME (def: def) => which def
  | NONE => []);

fun all_specifications_of (Defs defs) =
  (map o apsnd) (map snd o Inttab.dest o #specs) (Symtab.dest defs);

fun specifications_of (Defs defs) = lookup_list (map snd o Inttab.dest o #specs) defs;

val restricts_of = lookup_list #restricts;
val reducts_of = lookup_list #reducts;

fun dest (Defs defs) =
  let
    val restricts = Symtab.fold (fn (c, {restricts, ...}) =>
      fold (fn (args, description) => cons ((c, args), description)) restricts) defs [];
    val reducts = Symtab.fold (fn (c, {reducts, ...}) =>
      fold (fn (args, deps) => cons ((c, args), deps)) reducts) defs [];
  in {restricts = restricts, reducts = reducts} end;

val empty = Defs Symtab.empty;


(* specifications *)

fun disjoint_specs c (i, {description = a, pos = pos_a, lhs = Ts, ...}: spec) =
  Inttab.forall (fn (j, {description = b, pos = pos_b, lhs = Us, ...}: spec) =>
    i = j orelse disjoint_args (Ts, Us) orelse
      error ("Clash of specifications for constant " ^ quote c ^ ":\n" ^
        "  " ^ quote a ^ Position.here pos_a ^ "\n" ^
        "  " ^ quote b ^ Position.here pos_b));

fun join_specs c ({specs = specs1, restricts, reducts}, {specs = specs2, ...}: def) =
  let
    val specs' =
      Inttab.fold (fn spec2 => (disjoint_specs c spec2 specs1; Inttab.update spec2)) specs2 specs1;
  in make_def (specs', restricts, reducts) end;

fun update_specs c spec = map_def c (fn (specs, restricts, reducts) =>
  (disjoint_specs c spec specs; (Inttab.update spec specs, restricts, reducts)));


(* normalized dependencies: reduction with well-formedness check *)

local

val prt = Pretty.string_of oo pretty_const;
fun err ctxt (c, args) (d, Us) s1 s2 =
  error (s1 ^ " dependency of constant " ^ prt ctxt (c, args) ^ " -> " ^ prt ctxt (d, Us) ^ s2);

fun acyclic ctxt (c, args) (d, Us) =
  c <> d orelse
  is_none (match_args (args, Us)) orelse
  err ctxt (c, args) (d, Us) "Circular" "";

fun wellformed ctxt defs (c, args) (d, Us) =
  plain_args Us orelse
  (case find_first (fn (Ts, _) => not (disjoint_args (Ts, Us))) (restricts_of defs d) of
    SOME (Ts, description) =>
      err ctxt (c, args) (d, Us) "Malformed"
        ("\n(restriction " ^ prt ctxt (d, Ts) ^ " from " ^ quote description ^ ")")
  | NONE => true);

fun reduction ctxt defs const deps =
  let
    fun reduct Us (Ts, rhs) =
      (case match_args (Ts, Us) of
        NONE => NONE
      | SOME subst => SOME (map (apsnd (map subst)) rhs));
    fun reducts (d, Us) = get_first (reduct Us) (reducts_of defs d);

    val reds = map (`reducts) deps;
    val deps' =
      if forall (is_none o #1) reds then NONE
      else SOME (fold_rev
        (fn (NONE, dp) => insert (op =) dp | (SOME dps, _) => fold (insert (op =)) dps) reds []);
    val _ = forall (acyclic ctxt const) (the_default deps deps');
  in deps' end;

in

fun normalize ctxt =
  let
    fun norm_update (c, {reducts, ...}: def) (changed, defs) =
      let
        val reducts' = reducts |> map (fn (args, deps) =>
          (args, perhaps (reduction ctxt defs (c, args)) deps));
      in
        if reducts = reducts' then (changed, defs)
        else (true, defs |> map_def c (fn (specs, restricts, _) => (specs, restricts, reducts')))
      end;
    fun norm_all defs =
      (case Symtab.fold norm_update defs (false, defs) of
        (true, defs') => norm_all defs'
      | (false, _) => defs);
    fun check defs (c, {reducts, ...}: def) =
      reducts |> forall (fn (args, deps) => forall (wellformed ctxt defs (c, args)) deps);
  in norm_all #> (fn defs => tap (Symtab.forall (check defs)) defs) end;

fun dependencies ctxt (c, args) restr deps =
  map_def c (fn (specs, restricts, reducts) =>
    let
      val restricts' = Library.merge (op =) (restricts, restr);
      val reducts' = insert (op =) (args, deps) reducts;
    in (specs, restricts', reducts') end)
  #> normalize ctxt;

end;


(* merge *)

fun merge ctxt (Defs defs1, Defs defs2) =
  let
    fun add_deps (c, args) restr deps defs =
      if AList.defined (op =) (reducts_of defs c) args then defs
      else dependencies ctxt (c, args) restr deps defs;
    fun add_def (c, {restricts, reducts, ...}: def) =
      fold (fn (args, deps) => add_deps (c, args) restricts deps) reducts;
  in
    Defs (Symtab.join join_specs (defs1, defs2)
      |> normalize ctxt |> Symtab.fold add_def defs2)
  end;


(* define *)

fun define ctxt unchecked def description (c, args) deps (Defs defs) =
  let
    val pos = Position.thread_data ();
    val restr =
      if plain_args args orelse
        (case args of [Type (_, rec_args)] => plain_args rec_args | _ => false)
      then [] else [(args, description)];
    val spec =
      (serial (), {def = def, description = description, pos = pos, lhs = args, rhs = deps});
    val defs' = defs |> update_specs c spec;
  in Defs (defs' |> (if unchecked then I else dependencies ctxt (c, args) restr deps)) end;

end;
