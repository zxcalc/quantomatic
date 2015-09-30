(*  Title:      Pure/variable.ML
    Author:     Makarius

Fixed type/term variables and polymorphic term abbreviations.
*)

signature VARIABLE =
sig
  val names_of: Proof.context -> Name.context
  val binds_of: Proof.context -> (typ * term) Vartab.table
  val maxidx_of: Proof.context -> int
  val sorts_of: Proof.context -> sort list
  val constraints_of: Proof.context -> typ Vartab.table * sort Vartab.table
  val is_declared: Proof.context -> string -> bool
  val check_name: binding -> string
  val default_type: Proof.context -> string -> typ option
  val def_type: Proof.context -> bool -> indexname -> typ option
  val def_sort: Proof.context -> indexname -> sort option
  val declare_maxidx: int -> Proof.context -> Proof.context
  val declare_names: term -> Proof.context -> Proof.context
  val declare_constraints: term -> Proof.context -> Proof.context
  val declare_term: term -> Proof.context -> Proof.context
  val declare_typ: typ -> Proof.context -> Proof.context
  val declare_prf: Proofterm.proof -> Proof.context -> Proof.context
  val declare_thm: thm -> Proof.context -> Proof.context
  val variant_frees: Proof.context -> term list -> (string * 'a) list -> (string * 'a) list
  val bind_term: indexname * term option -> Proof.context -> Proof.context
  val expand_binds: Proof.context -> term -> term
  val lookup_const: Proof.context -> string -> string option
  val is_const: Proof.context -> string -> bool
  val declare_const: string * string -> Proof.context -> Proof.context
  val next_bound: string * typ -> Proof.context -> term * Proof.context
  val revert_bounds: Proof.context -> term -> term
  val is_body: Proof.context -> bool
  val set_body: bool -> Proof.context -> Proof.context
  val restore_body: Proof.context -> Proof.context -> Proof.context
  val improper_fixes: Proof.context -> Proof.context
  val restore_proper_fixes: Proof.context -> Proof.context -> Proof.context
  val is_improper: Proof.context -> string -> bool
  val is_fixed: Proof.context -> string -> bool
  val is_newly_fixed: Proof.context -> Proof.context -> string -> bool
  val fixed_ord: Proof.context -> string * string -> order
  val intern_fixed: Proof.context -> string -> string
  val markup_fixed: Proof.context -> string -> Markup.T
  val lookup_fixed: Proof.context -> string -> string option
  val revert_fixed: Proof.context -> string -> string
  val add_fixed_names: Proof.context -> term -> string list -> string list
  val add_fixed: Proof.context -> term -> (string * typ) list -> (string * typ) list
  val add_newly_fixed: Proof.context -> Proof.context ->
    term -> (string * typ) list -> (string * typ) list
  val add_free_names: Proof.context -> term -> string list -> string list
  val add_frees: Proof.context -> term -> (string * typ) list -> (string * typ) list
  val add_fixes_binding: binding list -> Proof.context -> string list * Proof.context
  val add_fixes: string list -> Proof.context -> string list * Proof.context
  val add_fixes_direct: string list -> Proof.context -> Proof.context
  val auto_fixes: term -> Proof.context -> Proof.context
  val fix_dummy_patterns: term -> Proof.context -> term * Proof.context
  val variant_fixes: string list -> Proof.context -> string list * Proof.context
  val dest_fixes: Proof.context -> (string * string) list
  val export_terms: Proof.context -> Proof.context -> term list -> term list
  val exportT_terms: Proof.context -> Proof.context -> term list -> term list
  val exportT: Proof.context -> Proof.context -> thm list -> thm list
  val export_prf: Proof.context -> Proof.context -> Proofterm.proof -> Proofterm.proof
  val export: Proof.context -> Proof.context -> thm list -> thm list
  val export_morphism: Proof.context -> Proof.context -> morphism
  val invent_types: sort list -> Proof.context -> (string * sort) list * Proof.context
  val importT_inst: term list -> Proof.context -> ((indexname * sort) * typ) list * Proof.context
  val import_inst: bool -> term list -> Proof.context ->
    (((indexname * sort) * typ) list * ((indexname * typ) * term) list) * Proof.context
  val importT_terms: term list -> Proof.context -> term list * Proof.context
  val import_terms: bool -> term list -> Proof.context -> term list * Proof.context
  val importT: thm list -> Proof.context -> ((ctyp * ctyp) list * thm list) * Proof.context
  val import_prf: bool -> Proofterm.proof -> Proof.context -> Proofterm.proof * Proof.context
  val import: bool -> thm list -> Proof.context ->
    (((ctyp * ctyp) list * (cterm * cterm) list) * thm list) * Proof.context
  val tradeT: (Proof.context -> thm list -> thm list) -> Proof.context -> thm list -> thm list
  val trade: (Proof.context -> thm list -> thm list) -> Proof.context -> thm list -> thm list
  val focus_params: term -> Proof.context -> (string list * (string * typ) list) * Proof.context
  val focus: term -> Proof.context -> ((string * (string * typ)) list * term) * Proof.context
  val focus_cterm: cterm -> Proof.context -> ((string * cterm) list * cterm) * Proof.context
  val focus_subgoal: int -> thm -> Proof.context -> ((string * cterm) list * cterm) * Proof.context
  val warn_extra_tfrees: Proof.context -> Proof.context -> unit
  val polymorphic_types: Proof.context -> term list -> (indexname * sort) list * term list
  val polymorphic: Proof.context -> term list -> term list
end;

structure Variable: VARIABLE =
struct

(** local context data **)

type fixes = (string * bool) Name_Space.table;
val empty_fixes: fixes = Name_Space.empty_table Markup.fixedN;

datatype data = Data of
 {names: Name.context,                  (*type/term variable names*)
  consts: string Symtab.table,          (*consts within the local scope*)
  bounds: int * ((string * typ) * string) list,  (*next index, internal name, type, external name*)
  fixes: fixes,                         (*term fixes -- global name space, intern ~> extern*)
  binds: (typ * term) Vartab.table,     (*term bindings*)
  type_occs: string list Symtab.table,  (*type variables -- possibly within term variables*)
  maxidx: int,                          (*maximum var index*)
  sorts: sort Ord_List.T,               (*declared sort occurrences*)
  constraints:
    typ Vartab.table *                  (*type constraints*)
    sort Vartab.table};                 (*default sorts*)

fun make_data (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =
  Data {names = names, consts = consts, bounds = bounds, fixes = fixes, binds = binds,
    type_occs = type_occs, maxidx = maxidx, sorts = sorts, constraints = constraints};

val empty_data =
  make_data (Name.context, Symtab.empty, (0, []), empty_fixes, Vartab.empty,
    Symtab.empty, ~1, [], (Vartab.empty, Vartab.empty));

structure Data = Proof_Data
(
  type T = data;
  fun init _ = empty_data;
);

fun map_data f =
  Data.map (fn Data {names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints} =>
    make_data (f (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints)));

fun map_names f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (f names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints));

fun map_consts f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, f consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints));

fun map_bounds f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, consts, f bounds, fixes, binds, type_occs, maxidx, sorts, constraints));

fun map_fixes f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, consts, bounds, f fixes, binds, type_occs, maxidx, sorts, constraints));

fun map_binds f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, consts, bounds, fixes, f binds, type_occs, maxidx, sorts, constraints));

fun map_type_occs f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, consts, bounds, fixes, binds, f type_occs, maxidx, sorts, constraints));

fun map_maxidx f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, consts, bounds, fixes, binds, type_occs, f maxidx, sorts, constraints));

fun map_sorts f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, consts, bounds, fixes, binds, type_occs, maxidx, f sorts, constraints));

fun map_constraints f =
  map_data (fn (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, constraints) =>
    (names, consts, bounds, fixes, binds, type_occs, maxidx, sorts, f constraints));

fun rep_data ctxt = Data.get ctxt |> (fn Data rep => rep);

val names_of = #names o rep_data;
val fixes_of = #fixes o rep_data;
val fixes_space = Name_Space.space_of_table o fixes_of;
val binds_of = #binds o rep_data;
val type_occs_of = #type_occs o rep_data;
val maxidx_of = #maxidx o rep_data;
val sorts_of = #sorts o rep_data;
val constraints_of = #constraints o rep_data;

val is_declared = Name.is_declared o names_of;

val check_name = Name_Space.base_name o tap Binding.check;



(** declarations **)

(* default sorts and types *)

fun default_type ctxt x = Vartab.lookup (#1 (constraints_of ctxt)) (x, ~1);

fun def_type ctxt pattern xi =
  let val {binds, constraints = (types, _), ...} = rep_data ctxt in
    (case Vartab.lookup types xi of
      NONE =>
        if pattern then NONE
        else Vartab.lookup binds xi |> Option.map (Type.mark_polymorphic o #1)
    | some => some)
  end;

val def_sort = Vartab.lookup o #2 o constraints_of;


(* maxidx *)

val declare_maxidx = map_maxidx o Integer.max;


(* names *)

fun declare_type_names t =
  map_names (fold_types (fold_atyps Term.declare_typ_names) t) #>
  map_maxidx (fold_types Term.maxidx_typ t);

fun declare_names t =
  declare_type_names t #>
  map_names (fold_aterms Term.declare_term_frees t) #>
  map_maxidx (Term.maxidx_term t);


(* type occurrences *)

fun decl_type_occsT T = fold_atyps (fn TFree (a, _) => Symtab.default (a, []) | _ => I) T;

val decl_type_occs = fold_term_types
  (fn Free (x, _) => fold_atyps (fn TFree (a, _) => Symtab.insert_list (op =) (a, x) | _ => I)
    | _ => decl_type_occsT);

val declare_type_occsT = map_type_occs o fold_types decl_type_occsT;
val declare_type_occs = map_type_occs o decl_type_occs;


(* constraints *)

fun constrain_tvar (xi, raw_S) =
  let val S = #2 (Term_Position.decode_positionS raw_S)
  in if S = dummyS then Vartab.delete_safe xi else Vartab.update (xi, S) end;

fun declare_constraints t = map_constraints (fn (types, sorts) =>
  let
    val types' = fold_aterms
      (fn Free (x, T) => Vartab.update ((x, ~1), T)
        | Var v => Vartab.update v
        | _ => I) t types;
    val sorts' = (fold_types o fold_atyps)
      (fn TFree (x, S) => constrain_tvar ((x, ~1), S)
        | TVar v => constrain_tvar v
        | _ => I) t sorts;
  in (types', sorts') end)
  #> declare_type_occsT t
  #> declare_type_names t;


(* common declarations *)

fun declare_internal t =
  declare_names t #>
  declare_type_occs t #>
  map_sorts (Sorts.insert_term t);

fun declare_term t =
  declare_internal t #>
  declare_constraints t;

val declare_typ = declare_term o Logic.mk_type;

val declare_prf = Proofterm.fold_proof_terms declare_internal (declare_internal o Logic.mk_type);

val declare_thm = Thm.fold_terms declare_internal;


(* renaming term/type frees *)

fun variant_frees ctxt ts frees =
  let
    val names = names_of (fold declare_names ts ctxt);
    val xs = fst (fold_map Name.variant (map #1 frees) names);
  in xs ~~ map snd frees end;



(** term bindings **)

fun bind_term (xi, NONE) = map_binds (Vartab.delete_safe xi)
  | bind_term ((x, i), SOME t) =
      let
        val u = Term.close_schematic_term t;
        val U = Term.fastype_of u;
      in declare_term u #> map_binds (Vartab.update ((x, i), (U, u))) end;

fun expand_binds ctxt =
  let
    val binds = binds_of ctxt;
    val get = fn Var (xi, _) => Vartab.lookup binds xi | _ => NONE;
  in Envir.beta_norm o Envir.expand_term get end;



(** consts **)

val lookup_const = Symtab.lookup o #consts o rep_data;
val is_const = is_some oo lookup_const;

val declare_fixed = map_consts o Symtab.delete_safe;
val declare_const = map_consts o Symtab.update;



(** bounds **)

fun next_bound (a, T) ctxt =
  let
    val b = Name.bound (#1 (#bounds (rep_data ctxt)));
    val ctxt' = ctxt |> map_bounds (fn (next, bounds) => (next + 1, ((b, T), a) :: bounds));
  in (Free (b, T), ctxt') end;

fun revert_bounds ctxt t =
  (case #2 (#bounds (rep_data ctxt)) of
    [] => t
  | bounds =>
      let
        val names = Term.declare_term_names t (names_of ctxt);
        val xs = rev (#1 (fold_map Name.variant (rev (map #2 bounds)) names));
        fun subst ((b, T), _) x' = (Free (b, T), Syntax_Trans.mark_bound_abs (x', T));
      in Term.subst_atomic (map2 subst bounds xs) t end);



(** fixes **)

(* inner body mode *)

val inner_body =
  Config.bool (Config.declare ("inner_body", @{here}) (K (Config.Bool false)));

fun is_body ctxt = Config.get ctxt inner_body;
val set_body = Config.put inner_body;
fun restore_body ctxt = set_body (is_body ctxt);


(* proper mode *)

val proper_fixes =
  Config.bool (Config.declare ("proper_fixes", @{here}) (K (Config.Bool true)));

val improper_fixes = Config.put proper_fixes false;
fun restore_proper_fixes ctxt = Config.put proper_fixes (Config.get ctxt proper_fixes);

fun is_improper ctxt x =
  (case Name_Space.lookup (fixes_of ctxt) x of
    SOME (_, proper) => not proper
  | NONE => false);


(* specialized name space *)

val is_fixed = Name_Space.defined o fixes_of;
fun is_newly_fixed inner outer = is_fixed inner andf (not o is_fixed outer);

val fixed_ord = Name_Space.entry_ord o fixes_space;
val intern_fixed = Name_Space.intern o fixes_space;

fun lookup_fixed ctxt x =
  let val x' = intern_fixed ctxt x
  in if is_fixed ctxt x' then SOME x' else NONE end;

fun revert_fixed ctxt x =
  (case Name_Space.lookup (fixes_of ctxt) x of
    SOME (x', _) => if intern_fixed ctxt x' = x then x' else x
  | NONE => x);

fun markup_fixed ctxt x =
  Name_Space.markup (fixes_space ctxt) x
  |> Markup.name (revert_fixed ctxt x);

fun dest_fixes ctxt =
  Name_Space.fold_table (fn (x, (y, _)) => cons (y, x)) (fixes_of ctxt) []
  |> sort (Name_Space.entry_ord (fixes_space ctxt) o apply2 #2);


(* collect variables *)

fun add_free_names ctxt =
  fold_aterms (fn Free (x, _) => not (is_fixed ctxt x) ? insert (op =) x | _ => I);

fun add_frees ctxt =
  fold_aterms (fn Free (x, T) => not (is_fixed ctxt x) ? insert (op =) (x, T) | _ => I);

fun add_fixed_names ctxt =
  fold_aterms (fn Free (x, _) => is_fixed ctxt x ? insert (op =) x | _ => I);

fun add_fixed ctxt =
  fold_aterms (fn Free (x, T) => is_fixed ctxt x ? insert (op =) (x, T) | _ => I);

fun add_newly_fixed ctxt' ctxt =
  fold_aterms (fn Free (x, T) => is_newly_fixed ctxt' ctxt x ? insert (op =) (x, T) | _ => I);


(* declarations *)

local

fun err_dups dups =
  error ("Duplicate fixed variable(s): " ^ commas (map Binding.print dups));

fun new_fixed ((x, x'), pos) ctxt =
  if is_some (lookup_fixed ctxt x') then err_dups [Binding.make (x, pos)]
  else
    let
      val proper = Config.get ctxt proper_fixes;
      val context = Context.Proof ctxt |> Name_Space.map_naming (K Name_Space.global_naming);
    in
      ctxt
      |> map_fixes
        (Name_Space.define context true (Binding.make (x', pos), (x, proper)) #> snd #>
          Name_Space.alias_table Name_Space.global_naming (Binding.make (x, pos)) x')
      |> declare_fixed x
      |> declare_constraints (Syntax.free x')
  end;

fun new_fixes names' xs xs' ps =
  map_names (K names') #>
  fold new_fixed ((xs ~~ xs') ~~ ps) #>
  pair xs';

in

fun add_fixes_binding bs ctxt =
  let
    val _ =
      (case filter (Name.is_skolem o Binding.name_of) bs of
        [] => ()
      | bads => error ("Illegal internal Skolem constant(s): " ^ commas (map Binding.print bads)));
    val _ =
      (case duplicates (op = o apply2 Binding.name_of) bs of
        [] => ()
      | dups => err_dups dups);

    val xs = map check_name bs;
    val names = names_of ctxt;
    val (xs', names') =
      if is_body ctxt then fold_map Name.variant xs names |>> map Name.skolem
      else (xs, fold Name.declare xs names);
  in ctxt |> new_fixes names' xs xs' (map Binding.pos_of bs) end;

fun variant_fixes raw_xs ctxt =
  let
    val names = names_of ctxt;
    val xs = map (fn x => Name.clean x |> Name.is_internal x ? Name.internal) raw_xs;
    val (xs', names') = fold_map Name.variant xs names |>> (is_body ctxt ? map Name.skolem);
  in ctxt |> new_fixes names' xs xs' (replicate (length xs) Position.none) end;

end;

val add_fixes = add_fixes_binding o map Binding.name;

fun add_fixes_direct xs ctxt = ctxt
  |> set_body false
  |> (snd o add_fixes xs)
  |> restore_body ctxt;

fun auto_fixes t ctxt = ctxt
  |> not (is_body ctxt) ? add_fixes_direct (rev (add_free_names ctxt t []))
  |> declare_term t;


(* dummy patterns *)

fun fix_dummy_patterns (Const ("Pure.dummy_pattern", T)) ctxt =
      let val ([x], ctxt') = add_fixes [Name.uu_] ctxt
      in (Free (x, T), ctxt') end
  | fix_dummy_patterns (Abs (x, T, b)) ctxt =
      let val (b', ctxt') = fix_dummy_patterns b ctxt
      in (Abs (x, T, b'), ctxt') end
  | fix_dummy_patterns (t $ u) ctxt =
      let
        val (t', ctxt') = fix_dummy_patterns t ctxt;
        val (u', ctxt'') = fix_dummy_patterns u ctxt';
      in (t' $ u', ctxt'') end
  | fix_dummy_patterns a ctxt = (a, ctxt);



(** export -- generalize type/term variables (beware of closure sizes) **)

fun export_inst inner outer =
  let
    val declared_outer = is_declared outer;
    val still_fixed = not o is_newly_fixed inner outer;

    val gen_fixes =
      Name_Space.fold_table (fn (y, _) => not (is_fixed outer y) ? cons y)
        (fixes_of inner) [];

    val type_occs_inner = type_occs_of inner;
    fun gen_fixesT ts =
      Symtab.fold (fn (a, xs) =>
        if declared_outer a orelse exists still_fixed xs
        then I else cons a) (fold decl_type_occs ts type_occs_inner) [];
  in (gen_fixesT, gen_fixes) end;

fun exportT_inst inner outer = #1 (export_inst inner outer);

fun exportT_terms inner outer =
  let
    val mk_tfrees = exportT_inst inner outer;
    val maxidx = maxidx_of outer;
  in
    fn ts => ts |> map
      (Term_Subst.generalize (mk_tfrees ts, [])
        (fold (Term.fold_types Term.maxidx_typ) ts maxidx + 1))
  end;

fun export_terms inner outer =
  let
    val (mk_tfrees, tfrees) = export_inst inner outer;
    val maxidx = maxidx_of outer;
  in
    fn ts => ts |> map
      (Term_Subst.generalize (mk_tfrees ts, tfrees)
        (fold Term.maxidx_term ts maxidx + 1))
  end;

fun export_prf inner outer prf =
  let
    val (mk_tfrees, frees) = export_inst (declare_prf prf inner) outer;
    val tfrees = mk_tfrees [];
    val maxidx = maxidx_of outer;
    val idx = Proofterm.maxidx_proof prf maxidx + 1;
    val gen_term = Term_Subst.generalize_same (tfrees, frees) idx;
    val gen_typ = Term_Subst.generalizeT_same tfrees idx;
  in Same.commit (Proofterm.map_proof_terms_same gen_term gen_typ) prf end;


fun gen_export (mk_tfrees, frees) maxidx ths =
  let
    val tfrees = mk_tfrees (map Thm.full_prop_of ths);
    val idx = fold Thm.maxidx_thm ths maxidx + 1;
  in map (Thm.generalize (tfrees, frees) idx) ths end;

fun exportT inner outer = gen_export (exportT_inst inner outer, []) (maxidx_of outer);
fun export inner outer = gen_export (export_inst inner outer) (maxidx_of outer);

fun export_morphism inner outer =
  let
    val fact = export inner outer;
    val term = singleton (export_terms inner outer);
    val typ = Logic.type_map term;
  in
    Morphism.morphism "Variable.export" {binding = [], typ = [typ], term = [term], fact = [fact]}
  end;



(** import -- fix schematic type/term variables **)

fun invent_types Ss ctxt =
  let
    val tfrees = Name.invent (names_of ctxt) Name.aT (length Ss) ~~ Ss;
    val ctxt' = fold (declare_constraints o Logic.mk_type o TFree) tfrees ctxt;
  in (tfrees, ctxt') end;

fun importT_inst ts ctxt =
  let
    val tvars = rev (fold Term.add_tvars ts []);
    val (tfrees, ctxt') = invent_types (map #2 tvars) ctxt;
  in (tvars ~~ map TFree tfrees, ctxt') end;

fun import_inst is_open ts ctxt =
  let
    val ren = Name.clean #> (if is_open then I else Name.internal);
    val (instT, ctxt') = importT_inst ts ctxt;
    val vars = map (apsnd (Term_Subst.instantiateT instT)) (rev (fold Term.add_vars ts []));
    val (xs, ctxt'') = variant_fixes (map (ren o #1 o #1) vars) ctxt';
    val inst = vars ~~ map Free (xs ~~ map #2 vars);
  in ((instT, inst), ctxt'') end;

fun importT_terms ts ctxt =
  let val (instT, ctxt') = importT_inst ts ctxt
  in (map (Term_Subst.instantiate (instT, [])) ts, ctxt') end;

fun import_terms is_open ts ctxt =
  let val (inst, ctxt') = import_inst is_open ts ctxt
  in (map (Term_Subst.instantiate inst) ts, ctxt') end;

fun importT ths ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val (instT, ctxt') = importT_inst (map Thm.full_prop_of ths) ctxt;
    val insts' as (instT', _) = Thm.certify_inst thy (instT, []);
    val ths' = map (Thm.instantiate insts') ths;
  in ((instT', ths'), ctxt') end;

fun import_prf is_open prf ctxt =
  let
    val ts = rev (Proofterm.fold_proof_terms cons (cons o Logic.mk_type) prf []);
    val (insts, ctxt') = import_inst is_open ts ctxt;
  in (Proofterm.instantiate insts prf, ctxt') end;

fun import is_open ths ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val (insts, ctxt') = import_inst is_open (map Thm.full_prop_of ths) ctxt;
    val insts' = Thm.certify_inst thy insts;
    val ths' = map (Thm.instantiate insts') ths;
  in ((insts', ths'), ctxt') end;


(* import/export *)

fun gen_trade imp exp f ctxt ths =
  let val ((_, ths'), ctxt') = imp ths ctxt
  in exp ctxt' ctxt (f ctxt' ths') end;

val tradeT = gen_trade importT exportT;
val trade = gen_trade (import true) export;


(* focus on outermost parameters: !!x y z. B *)

fun focus_params t ctxt =
  let
    val (xs, Ts) =
      split_list (Term.variant_frees t (Term.strip_all_vars t));  (*as they are printed :-*)
    val (xs', ctxt') = variant_fixes xs ctxt;
    val ps = xs' ~~ Ts;
    val ctxt'' = ctxt' |> fold (declare_constraints o Free) ps;
  in ((xs, ps), ctxt'') end;

fun focus t ctxt =
  let
    val ((xs, ps), ctxt') = focus_params t ctxt;
    val t' = Term.subst_bounds (rev (map Free ps), Term.strip_all_body t);
  in (((xs ~~ ps), t'), ctxt') end;

fun forall_elim_prop t prop =
  Thm.beta_conversion false (Thm.apply (Thm.dest_arg prop) t)
  |> Thm.cprop_of |> Thm.dest_arg;

fun focus_cterm goal ctxt =
  let
    val ((xs, ps), ctxt') = focus_params (Thm.term_of goal) ctxt;
    val ps' = map (Thm.cterm_of ctxt' o Free) ps;
    val goal' = fold forall_elim_prop ps' goal;
  in ((xs ~~ ps', goal'), ctxt') end;

fun focus_subgoal i st =
  let
    val all_vars = Thm.fold_terms Term.add_vars st [];
    val no_binds = map (fn (xi, _) => (xi, NONE)) all_vars;
  in
    fold bind_term no_binds #>
    fold (declare_constraints o Var) all_vars #>
    focus_cterm (Thm.cprem_of st i)
  end;



(** implicit polymorphism **)

(* warn_extra_tfrees *)

fun warn_extra_tfrees ctxt1 ctxt2 =
  let
    fun occs_typ a = Term.exists_subtype (fn TFree (b, _) => a = b | _ => false);
    fun occs_free a x =
      (case def_type ctxt1 false (x, ~1) of
        SOME T => if occs_typ a T then I else cons (a, x)
      | NONE => cons (a, x));

    val occs1 = type_occs_of ctxt1;
    val occs2 = type_occs_of ctxt2;
    val extras = Symtab.fold (fn (a, xs) =>
      if Symtab.defined occs1 a then I else fold (occs_free a) xs) occs2 [];
    val tfrees = map #1 extras |> sort_distinct string_ord;
    val frees = map #2 extras |> sort_distinct string_ord;
  in
    if null extras orelse not (Context_Position.is_visible ctxt2) then ()
    else warning ("Introduced fixed type variable(s): " ^ commas tfrees ^ " in " ^
      space_implode " or " (map quote frees))
  end;


(* polymorphic terms *)

fun polymorphic_types ctxt ts =
  let
    val ctxt' = fold declare_term ts ctxt;
    val occs = type_occs_of ctxt;
    val occs' = type_occs_of ctxt';
    val types = Symtab.fold (fn (a, _) => if Symtab.defined occs a then I else cons a) occs' [];
    val idx = maxidx_of ctxt' + 1;
    val Ts' = (fold o fold_types o fold_atyps)
      (fn T as TFree _ =>
          (case Term_Subst.generalizeT types idx T of TVar v => insert (op =) v | _ => I)
        | _ => I) ts [];
    val ts' = map (Term_Subst.generalize (types, []) idx) ts;
  in (rev Ts', ts') end;

fun polymorphic ctxt ts = snd (polymorphic_types ctxt ts);

end;
