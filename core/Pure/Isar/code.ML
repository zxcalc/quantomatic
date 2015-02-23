(*  Title:      Pure/Isar/code.ML
    Author:     Florian Haftmann, TU Muenchen

Abstract executable ingredients of theory.  Management of data
dependent on executable ingredients as synchronized cache; purged
on any change of underlying executable ingredients.
*)

signature CODE =
sig
  (*constants*)
  val check_const: theory -> term -> string
  val read_bare_const: theory -> string -> string * typ
  val read_const: theory -> string -> string
  val string_of_const: theory -> string -> string
  val const_typ: theory -> string -> typ
  val args_number: theory -> string -> int

  (*constructor sets*)
  val constrset_of_consts: theory -> (string * typ) list
    -> string * ((string * sort) list * (string * ((string * sort) list * typ list)) list)

  (*code equations and certificates*)
  val mk_eqn: theory -> thm * bool -> thm * bool
  val mk_eqn_liberal: theory -> thm -> (thm * bool) option
  val assert_eqn: theory -> thm * bool -> thm * bool
  val assert_abs_eqn: theory -> string option -> thm -> thm * string
  val const_typ_eqn: theory -> thm -> string * typ
  val expand_eta: theory -> int -> thm -> thm
  type cert
  val constrain_cert: theory -> sort list -> cert -> cert
  val conclude_cert: cert -> cert
  val typargs_deps_of_cert: theory -> cert -> (string * sort) list * (string * typ list) list
  val equations_of_cert: theory -> cert -> ((string * sort) list * typ)
    * (((term * string option) list * (term * string option)) * (thm option * bool)) list option
  val pretty_cert: theory -> cert -> Pretty.T list

  (*executable code*)
  val add_datatype: (string * typ) list -> theory -> theory
  val add_datatype_cmd: string list -> theory -> theory
  val datatype_interpretation:
    (string * ((string * sort) list * (string * ((string * sort) list * typ list)) list)
      -> theory -> theory) -> theory -> theory
  val add_abstype: thm -> theory -> theory
  val abstype_interpretation:
    (string * ((string * sort) list * ((string * ((string * sort) list * typ)) * (string * thm)))
      -> theory -> theory) -> theory -> theory
  val add_eqn: thm -> theory -> theory
  val add_nbe_eqn: thm -> theory -> theory
  val add_abs_eqn: thm -> theory -> theory
  val add_abs_eqn_attribute: attribute
  val add_abs_eqn_attrib: Attrib.src
  val add_default_eqn: thm -> theory -> theory
  val add_default_eqn_attribute: attribute
  val add_default_eqn_attrib: Attrib.src
  val add_nbe_default_eqn: thm -> theory -> theory
  val add_nbe_default_eqn_attribute: attribute
  val add_nbe_default_eqn_attrib: Attrib.src
  val del_eqn: thm -> theory -> theory
  val del_eqns: string -> theory -> theory
  val del_exception: string -> theory -> theory
  val add_case: thm -> theory -> theory
  val add_undefined: string -> theory -> theory
  val get_type: theory -> string
    -> ((string * sort) list * (string * ((string * sort) list * typ list)) list) * bool
  val get_type_of_constr_or_abstr: theory -> string -> (string * bool) option
  val is_constr: theory -> string -> bool
  val is_abstr: theory -> string -> bool
  val get_cert: Proof.context -> ((thm * bool) list -> (thm * bool) list option) list
    -> string -> cert
  val get_case_scheme: theory -> string -> (int * (int * string option list)) option
  val get_case_cong: theory -> string -> thm option
  val undefineds: theory -> string list
  val print_codesetup: theory -> unit
end;

signature CODE_DATA_ARGS =
sig
  type T
  val empty: T
end;

signature CODE_DATA =
sig
  type T
  val change: theory option -> (T -> T) -> T
  val change_yield: theory option -> (T -> 'a * T) -> 'a * T
end;

signature PRIVATE_CODE =
sig
  include CODE
  val declare_data: Any.T -> serial
  val change_yield_data: serial * ('a -> Any.T) * (Any.T -> 'a)
    -> theory -> ('a -> 'b * 'a) -> 'b * 'a
end;

structure Code : PRIVATE_CODE =
struct

(** auxiliary **)

(* printing *)

fun string_of_typ thy =
  Syntax.string_of_typ (Config.put show_sorts true (Syntax.init_pretty_global thy));

fun string_of_const thy c =
  let val ctxt = Proof_Context.init_global thy in
    case Axclass.inst_of_param thy c of
      SOME (c, tyco) =>
        Proof_Context.extern_const ctxt c ^ " " ^ enclose "[" "]"
          (Proof_Context.extern_type ctxt tyco)
    | NONE => Proof_Context.extern_const ctxt c
  end;


(* constants *)

fun const_typ thy = Type.strip_sorts o Sign.the_const_type thy;

fun args_number thy = length o binder_types o const_typ thy;

fun devarify ty =
  let
    val tys = fold_atyps (fn TVar vi_sort => AList.update (op =) vi_sort) ty [];
    val vs = Name.invent Name.context Name.aT (length tys);
    val mapping = map2 (fn v => fn (vi, sort) => (vi, TFree (v, sort))) vs tys;
  in Term.typ_subst_TVars mapping ty end;

fun typscheme thy (c, ty) =
  (map dest_TFree (Sign.const_typargs thy (c, ty)), Type.strip_sorts ty);

fun typscheme_equiv (ty1, ty2) =
  Type.raw_instance (devarify ty1, ty2) andalso Type.raw_instance (devarify ty2, ty1);

fun check_bare_const thy t = case try dest_Const t
 of SOME c_ty => c_ty
  | NONE => error ("Not a constant: " ^ Syntax.string_of_term_global thy t);

fun check_unoverload thy (c, ty) =
  let
    val c' = Axclass.unoverload_const thy (c, ty);
    val ty_decl = const_typ thy c';
  in
    if typscheme_equiv (ty_decl, Logic.varifyT_global ty)
    then c'
    else
      error ("Type\n" ^ string_of_typ thy ty ^
        "\nof constant " ^ quote c ^
        "\nis too specific compared to declared type\n" ^
        string_of_typ thy ty_decl)
  end; 

fun check_const thy = check_unoverload thy o check_bare_const thy;

fun read_bare_const thy = check_bare_const thy o Syntax.read_term_global thy;

fun read_const thy = check_unoverload thy o read_bare_const thy;


(** data store **)

(* datatypes *)

datatype typ_spec = Constructors of (string * ((string * sort) list * typ list)) list *
      string list (*references to associated case constructors*)
  | Abstractor of (string * ((string * sort) list * typ)) * (string * thm);

fun constructors_of (Constructors (cos, _)) = (cos, false)
  | constructors_of (Abstractor ((co, (vs, ty)), _)) = ([(co, (vs, [ty]))], true);

fun case_consts_of (Constructors (_, case_consts)) = case_consts
  | case_consts_of (Abstractor _) = [];

(* functions *)

datatype fun_spec = Default of (thm * bool) list * (thm * bool) list lazy
      (* (cache for default equations, lazy computation of default equations)
         -- helps to restore natural order of default equations *)
  | Eqns of (thm * bool) list
  | None
  | Proj of term * string
  | Abstr of thm * string;

val initial_fun_spec = Default ([], Lazy.value []);

fun is_default (Default _) = true
  | is_default _ = false;

fun associated_abstype (Abstr (_, tyco)) = SOME tyco
  | associated_abstype _ = NONE;


(* executable code data *)

datatype spec = Spec of {
  history_concluded: bool,
  functions: ((bool * fun_spec) * (serial * fun_spec) list) Symtab.table
    (*with explicit history*),
  types: ((serial * ((string * sort) list * typ_spec)) list) Symtab.table
    (*with explicit history*),
  cases: ((int * (int * string option list)) * thm) Symtab.table * unit Symtab.table
};

fun make_spec (history_concluded, (functions, (types, cases))) =
  Spec { history_concluded = history_concluded, functions = functions, types = types, cases = cases };
fun map_spec f (Spec { history_concluded = history_concluded,
  functions = functions, types = types, cases = cases }) =
  make_spec (f (history_concluded, (functions, (types, cases))));
fun merge_spec (Spec { history_concluded = _, functions = functions1,
    types = types1, cases = (cases1, undefs1) },
  Spec { history_concluded = _, functions = functions2,
    types = types2, cases = (cases2, undefs2) }) =
  let
    val types = Symtab.join (K (AList.merge (op =) (K true))) (types1, types2);
    val case_consts_of' = (maps case_consts_of o map (snd o snd o hd o snd) o Symtab.dest);
    fun merge_functions ((_, history1), (_, history2)) =
      let
        val raw_history = AList.merge (op = : serial * serial -> bool)
          (K true) (history1, history2);
        val filtered_history = filter_out (is_default o snd) raw_history;
        val history = if null filtered_history
          then raw_history else filtered_history;
      in ((false, (snd o hd) history), history) end;
    val all_datatype_specs = map (snd o snd o hd o snd) (Symtab.dest types);
    val all_constructors = maps (map fst o fst o constructors_of) all_datatype_specs;
    val invalidated_case_consts = union (op =) (case_consts_of' types1) (case_consts_of' types2)
      |> subtract (op =) (maps case_consts_of all_datatype_specs)
    val functions = Symtab.join (K merge_functions) (functions1, functions2)
      |> fold (fn c => Symtab.map_entry c (apfst (K (true, initial_fun_spec)))) all_constructors;
    val cases = (Symtab.merge (K true) (cases1, cases2)
      |> fold Symtab.delete invalidated_case_consts, Symtab.merge (K true) (undefs1, undefs2));
  in make_spec (false, (functions, (types, cases))) end;

fun history_concluded (Spec { history_concluded, ... }) = history_concluded;
fun the_functions (Spec { functions, ... }) = functions;
fun the_types (Spec { types, ... }) = types;
fun the_cases (Spec { cases, ... }) = cases;
val map_history_concluded = map_spec o apfst;
val map_functions = map_spec o apsnd o apfst;
val map_typs = map_spec o apsnd o apsnd o apfst;
val map_cases = map_spec o apsnd o apsnd o apsnd;


(* data slots dependent on executable code *)

(*private copy avoids potential conflict of table exceptions*)
structure Datatab = Table(type key = int val ord = int_ord);

local

type kind = { empty: Any.T };

val kinds = Synchronized.var "Code_Data" (Datatab.empty: kind Datatab.table);

fun invoke f k =
  (case Datatab.lookup (Synchronized.value kinds) k of
    SOME kind => f kind
  | NONE => raise Fail "Invalid code data identifier");

in

fun declare_data empty =
  let
    val k = serial ();
    val kind = { empty = empty };
    val _ = Synchronized.change kinds (Datatab.update (k, kind));
  in k end;

fun invoke_init k = invoke (fn kind => #empty kind) k;

end; (*local*)


(* theory store *)

local

type data = Any.T Datatab.table;
fun empty_dataref () = Synchronized.var "code data" (NONE : (data * theory) option);

structure Code_Data = Theory_Data
(
  type T = spec * (data * theory) option Synchronized.var;
  val empty = (make_spec (false, (Symtab.empty,
    (Symtab.empty, (Symtab.empty, Symtab.empty)))), empty_dataref ());
  val extend : T -> T = apsnd (K (empty_dataref ()));
  fun merge ((spec1, _), (spec2, _)) =
    (merge_spec (spec1, spec2), empty_dataref ());
);

in


(* access to executable code *)

val the_exec : theory -> spec = fst o Code_Data.get;

fun map_exec_purge f = Code_Data.map (fn (exec, _) => (f exec, empty_dataref ()));

fun change_fun_spec c f = (map_exec_purge o map_functions
  o (Symtab.map_default (c, ((false, initial_fun_spec), [])))
    o apfst) (fn (_, spec) => (true, f spec));


(* tackling equation history *)

fun continue_history thy = if (history_concluded o the_exec) thy
  then thy
    |> (Code_Data.map o apfst o map_history_concluded) (K false)
    |> SOME
  else NONE;

fun conclude_history thy = if (history_concluded o the_exec) thy
  then NONE
  else thy
    |> (Code_Data.map o apfst)
        ((map_functions o Symtab.map) (fn _ => fn ((changed, current), history) =>
          ((false, current),
            if changed then (serial (), current) :: history else history))
        #> map_history_concluded (K true))
    |> SOME;

val _ = Theory.setup
  (Theory.at_begin continue_history #> Theory.at_end conclude_history);


(* access to data dependent on abstract executable code *)

fun change_yield_data (kind, mk, dest) theory f =
  let
    val dataref = (snd o Code_Data.get) theory;
    val (datatab, thy) = case Synchronized.value dataref
     of SOME (datatab, thy) =>
        if Theory.eq_thy (theory, thy)
          then (datatab, thy)
          else (Datatab.empty, theory)
      | NONE => (Datatab.empty, theory)
    val data = case Datatab.lookup datatab kind
     of SOME data => data
      | NONE => invoke_init kind;
    val result as (_, data') = f (dest data);
    val _ = Synchronized.change dataref
      ((K o SOME) (Datatab.update (kind, mk data') datatab, thy));
  in result end;

end; (*local*)


(** foundation **)

(* datatypes *)

fun no_constr thy s (c, ty) = error ("Not a datatype constructor:\n" ^ string_of_const thy c
  ^ " :: " ^ string_of_typ thy ty ^ "\n" ^ enclose "(" ")" s);

fun analyze_constructor thy (c, ty) =
  let
    val _ = Thm.cterm_of thy (Const (c, ty));
    val ty_decl = devarify (const_typ thy c);
    fun last_typ c_ty ty =
      let
        val tfrees = Term.add_tfreesT ty [];
        val (tyco, vs) = (apsnd o map) dest_TFree (dest_Type (body_type ty))
          handle TYPE _ => no_constr thy "bad type" c_ty
        val _ = if tyco = "fun" then no_constr thy "bad type" c_ty else ();
        val _ =
          if has_duplicates (eq_fst (op =)) vs
          then no_constr thy "duplicate type variables in datatype" c_ty else ();
        val _ =
          if length tfrees <> length vs
          then no_constr thy "type variables missing in datatype" c_ty else ();
      in (tyco, vs) end;
    val (tyco, _) = last_typ (c, ty) ty_decl;
    val (_, vs) = last_typ (c, ty) ty;
  in ((tyco, map snd vs), (c, (map fst vs, ty))) end;

fun constrset_of_consts thy consts =
  let
    val _ = map (fn (c, _) => if (is_some o Axclass.class_of_param thy) c
      then error ("Is a class parameter: " ^ string_of_const thy c) else ()) consts;
    val raw_constructors = map (analyze_constructor thy) consts;
    val tyco = case distinct (op =) (map (fst o fst) raw_constructors)
     of [tyco] => tyco
      | [] => error "Empty constructor set"
      | tycos => error ("Different type constructors in constructor set: " ^ commas_quote tycos)
    val vs = Name.invent Name.context Name.aT (Sign.arity_number thy tyco);
    fun inst vs' (c, (vs, ty)) =
      let
        val the_v = the o AList.lookup (op =) (vs ~~ vs');
        val ty' = map_type_tfree (fn (v, _) => TFree (the_v v, [])) ty;
        val (vs'', ty'') = typscheme thy (c, ty');
      in (c, (vs'', binder_types ty'')) end;
    val constructors = map (inst vs o snd) raw_constructors;
  in (tyco, (map (rpair []) vs, constructors)) end;

fun get_type_entry thy tyco = case these (Symtab.lookup ((the_types o the_exec) thy) tyco)
 of (_, entry) :: _ => SOME entry
  | _ => NONE;

fun get_type thy tyco = case get_type_entry thy tyco
 of SOME (vs, spec) => apfst (pair vs) (constructors_of spec)
  | NONE => Sign.arity_number thy tyco
      |> Name.invent Name.context Name.aT
      |> map (rpair [])
      |> rpair []
      |> rpair false;

fun get_abstype_spec thy tyco = case get_type_entry thy tyco
 of SOME (vs, Abstractor spec) => (vs, spec)
  | _ => error ("Not an abstract type: " ^ tyco);
 
fun get_type_of_constr_or_abstr thy c =
  case (body_type o const_typ thy) c
   of Type (tyco, _) => let val ((_, cos), abstract) = get_type thy tyco
        in if member (op =) (map fst cos) c then SOME (tyco, abstract) else NONE end
    | _ => NONE;

fun is_constr thy c = case get_type_of_constr_or_abstr thy c
 of SOME (_, false) => true
   | _ => false;

fun is_abstr thy c = case get_type_of_constr_or_abstr thy c
 of SOME (_, true) => true
   | _ => false;


(* bare code equations *)

(* convention for variables:
    ?x ?'a   for free-floating theorems (e.g. in the data store)
    ?x  'a   for certificates
     x  'a   for final representation of equations
*)

exception BAD_THM of string;
fun bad_thm msg = raise BAD_THM msg;
fun error_thm f thy (thm, proper) = f (thm, proper)
  handle BAD_THM msg => error (msg ^ ", in theorem:\n" ^ Display.string_of_thm_global thy thm);
fun error_abs_thm f thy thm = f thm
  handle BAD_THM msg => error (msg ^ ", in theorem:\n" ^ Display.string_of_thm_global thy thm);
fun warning_thm f thy (thm, proper) = SOME (f (thm, proper))
  handle BAD_THM msg => (warning (msg ^ ", in theorem:\n" ^ Display.string_of_thm_global thy thm); NONE)
fun try_thm f thm_proper = SOME (f thm_proper)
  handle BAD_THM _ => NONE;

fun is_linear thm =
  let val (_, args) = (strip_comb o fst o Logic.dest_equals o Thm.plain_prop_of) thm
  in not (has_duplicates (op =) ((fold o fold_aterms)
    (fn Var (v, _) => cons v | _ => I) args [])) end;

fun check_decl_ty thy (c, ty) =
  let
    val ty_decl = const_typ thy c;
  in if typscheme_equiv (ty_decl, ty) then ()
    else bad_thm ("Type\n" ^ string_of_typ thy ty
      ^ "\nof constant " ^ quote c
      ^ "\nis too specific compared to declared type\n"
      ^ string_of_typ thy ty_decl)
  end; 

fun check_eqn thy { allow_nonlinear, allow_consts, allow_pats } thm (lhs, rhs) =
  let
    fun vars_of t = fold_aterms (fn Var (v, _) => insert (op =) v
      | Free _ => bad_thm "Illegal free variable"
      | _ => I) t [];
    fun tvars_of t = fold_term_types (fn _ =>
      fold_atyps (fn TVar (v, _) => insert (op =) v
        | TFree _ => bad_thm "Illegal free type variable")) t [];
    val lhs_vs = vars_of lhs;
    val rhs_vs = vars_of rhs;
    val lhs_tvs = tvars_of lhs;
    val rhs_tvs = tvars_of rhs;
    val _ = if null (subtract (op =) lhs_vs rhs_vs)
      then ()
      else bad_thm "Free variables on right hand side of equation";
    val _ = if null (subtract (op =) lhs_tvs rhs_tvs)
      then ()
      else bad_thm "Free type variables on right hand side of equation";
    val (head, args) = strip_comb lhs;
    val (c, ty) = case head
     of Const (c_ty as (_, ty)) => (Axclass.unoverload_const thy c_ty, ty)
      | _ => bad_thm "Equation not headed by constant";
    fun check _ (Abs _) = bad_thm "Abstraction on left hand side of equation"
      | check 0 (Var _) = ()
      | check _ (Var _) = bad_thm "Variable with application on left hand side of equation"
      | check n (t1 $ t2) = (check (n+1) t1; check 0 t2)
      | check n (Const (c_ty as (c, ty))) =
          if allow_pats then let
            val c' = Axclass.unoverload_const thy c_ty
          in if n = (length o binder_types) ty
            then if allow_consts orelse is_constr thy c'
              then ()
              else bad_thm (quote c ^ " is not a constructor, on left hand side of equation")
            else bad_thm ("Partially applied constant " ^ quote c ^ " on left hand side of equation")
          end else bad_thm ("Pattern not allowed here, but constant " ^ quote c ^ " encountered on left hand side of equation")
    val _ = map (check 0) args;
    val _ = if allow_nonlinear orelse is_linear thm then ()
      else bad_thm "Duplicate variables on left hand side of equation";
    val _ = if (is_none o Axclass.class_of_param thy) c then ()
      else bad_thm "Overloaded constant as head in equation";
    val _ = if not (is_constr thy c) then ()
      else bad_thm "Constructor as head in equation";
    val _ = if not (is_abstr thy c) then ()
      else bad_thm "Abstractor as head in equation";
    val _ = check_decl_ty thy (c, ty);
    val _ = case strip_type ty
     of (Type (tyco, _) :: _, _) => (case get_type_entry thy tyco
       of SOME (_, Abstractor (_, (proj, _))) => if c = proj
            then bad_thm "Projection as head in equation"
            else ()
        | _ => ())
      | _ => ();
  in () end;

fun gen_assert_eqn thy check_patterns (thm, proper) =
  let
    val (lhs, rhs) = (Logic.dest_equals o Thm.plain_prop_of) thm
      handle TERM _ => bad_thm "Not an equation"
           | THM _ => bad_thm "Not a proper equation";
    val _ = check_eqn thy { allow_nonlinear = not proper,
      allow_consts = not (proper andalso check_patterns), allow_pats = true } thm (lhs, rhs);
  in (thm, proper) end;

fun assert_abs_eqn thy some_tyco thm =
  let
    val (full_lhs, rhs) = (Logic.dest_equals o Thm.plain_prop_of) thm
      handle TERM _ => bad_thm "Not an equation"
           | THM _ => bad_thm "Not a proper equation";
    val (rep, lhs) = dest_comb full_lhs
      handle TERM _ => bad_thm "Not an abstract equation";
    val (rep_const, ty) = dest_Const rep
      handle TERM _ => bad_thm "Not an abstract equation";
    val (tyco, Ts) = (dest_Type o domain_type) ty
      handle TERM _ => bad_thm "Not an abstract equation"
           | TYPE _ => bad_thm "Not an abstract equation";
    val _ = case some_tyco of SOME tyco' => if tyco = tyco' then ()
          else bad_thm ("Abstract type mismatch:" ^ quote tyco ^ " vs. " ^ quote tyco')
      | NONE => ();
    val (vs', (_, (rep', _))) = case try (get_abstype_spec thy) tyco
     of SOME data => data
      | NONE => bad_thm ("Not an abstract type: " ^ tyco);
    val _ = if rep_const = rep' then ()
      else bad_thm ("Projection mismatch: " ^ quote rep_const ^ " vs. " ^ quote rep');
    val _ = check_eqn thy { allow_nonlinear = false,
      allow_consts = false, allow_pats = false } thm (lhs, rhs);
    val _ = if forall2 (fn T => fn (_, sort) => Sign.of_sort thy (T, sort)) Ts vs' then ()
      else error ("Type arguments do not satisfy sort constraints of abstype certificate.");
  in (thm, tyco) end;

fun assert_eqn thy = gen_assert_eqn thy true;

fun meta_rewrite thy = Local_Defs.meta_rewrite_rule (Proof_Context.init_global thy);

fun mk_eqn thy = error_thm (gen_assert_eqn thy false) thy o
  apfst (meta_rewrite thy);

fun mk_eqn_liberal thy = Option.map (fn (thm, _) => (thm, is_linear thm))
  o try_thm (gen_assert_eqn thy false) o rpair false o meta_rewrite thy;

fun mk_eqn_maybe_abs thy raw_thm =
  let
    val thm = meta_rewrite thy raw_thm;
    val some_abs_thm = try_thm (assert_abs_eqn thy NONE) thm;
  in case some_abs_thm
   of SOME (thm, tyco) => SOME ((thm, true), SOME tyco)
    | NONE => (Option.map (fn (thm, _) => ((thm, is_linear thm), NONE))
        o warning_thm (gen_assert_eqn thy false) thy) (thm, false)
  end;

fun mk_abs_eqn thy = error_abs_thm (assert_abs_eqn thy NONE) thy o meta_rewrite thy;

val head_eqn = dest_Const o fst o strip_comb o fst o Logic.dest_equals o Thm.plain_prop_of;

fun const_typ_eqn thy thm =
  let
    val (c, ty) = head_eqn thm;
    val c' = Axclass.unoverload_const thy (c, ty);
      (*permissive wrt. to overloaded constants!*)
  in (c', ty) end;

fun const_eqn thy = fst o const_typ_eqn thy;

fun const_abs_eqn thy = Axclass.unoverload_const thy o dest_Const o fst o strip_comb o snd
  o dest_comb o fst o Logic.dest_equals o Thm.plain_prop_of;

fun mk_proj tyco vs ty abs rep =
  let
    val ty_abs = Type (tyco, map TFree vs);
    val xarg = Var (("x", 0), ty);
  in Logic.mk_equals (Const (rep, ty_abs --> ty) $ (Const (abs, ty --> ty_abs) $ xarg), xarg) end;


(* technical transformations of code equations *)

fun expand_eta thy k thm =
  let
    val (lhs, rhs) = (Logic.dest_equals o Thm.plain_prop_of) thm;
    val (_, args) = strip_comb lhs;
    val l = if k = ~1
      then (length o fst o strip_abs) rhs
      else Int.max (0, k - length args);
    val (raw_vars, _) = Term.strip_abs_eta l rhs;
    val vars = burrow_fst (Name.variant_list (map (fst o fst) (Term.add_vars lhs [])))
      raw_vars;
    fun expand (v, ty) thm = Drule.fun_cong_rule thm
      (Thm.cterm_of thy (Var ((v, 0), ty)));
  in
    thm
    |> fold expand vars
    |> Conv.fconv_rule Drule.beta_eta_conversion
  end;

fun same_arity thy thms =
  let
    val num_args_of = length o snd o strip_comb o fst o Logic.dest_equals;
    val k = fold (Integer.max o num_args_of o Thm.prop_of) thms 0;
  in map (expand_eta thy k) thms end;

fun mk_desymbolization pre post mk vs =
  let
    val names = map (pre o fst o fst) vs
      |> map (Name.desymbolize (SOME false))
      |> Name.variant_list []
      |> map post;
  in map_filter (fn (((v, i), x), v') =>
    if v = v' andalso i = 0 then NONE
    else SOME (((v, i), x), mk ((v', 0), x))) (vs ~~ names)
  end;

fun desymbolize_tvars thms =
  let
    val tvs = fold (Term.add_tvars o Thm.prop_of) thms [];
    val tvar_subst = mk_desymbolization (unprefix "'") (prefix "'") TVar tvs;
  in map (Thm.certify_instantiate (tvar_subst, [])) thms end;

fun desymbolize_vars thm =
  let
    val vs = Term.add_vars (Thm.prop_of thm) [];
    val var_subst = mk_desymbolization I I Var vs;
  in Thm.certify_instantiate ([], var_subst) thm end;

fun canonize_thms thy = desymbolize_tvars #> same_arity thy #> map desymbolize_vars;


(* abstype certificates *)

fun check_abstype_cert thy proto_thm =
  let
    val thm = (Axclass.unoverload thy o meta_rewrite thy) proto_thm;
    val (lhs, rhs) = Logic.dest_equals (Thm.plain_prop_of thm)
      handle TERM _ => bad_thm "Not an equation"
           | THM _ => bad_thm "Not a proper equation";
    val ((abs, raw_ty), ((rep, rep_ty), param)) = (apsnd (apfst dest_Const o dest_comb)
        o apfst dest_Const o dest_comb) lhs
      handle TERM _ => bad_thm "Not an abstype certificate";
    val _ = pairself (fn c => if (is_some o Axclass.class_of_param thy) c
      then error ("Is a class parameter: " ^ string_of_const thy c) else ()) (abs, rep);
    val _ = check_decl_ty thy (abs, raw_ty);
    val _ = check_decl_ty thy (rep, rep_ty);
    val _ = if length (binder_types raw_ty) = 1
      then ()
      else bad_thm "Bad type for abstract constructor";
    val _ = (fst o dest_Var) param
      handle TERM _ => bad_thm "Not an abstype certificate";
    val _ = if param = rhs then () else bad_thm "Not an abstype certificate";
    val ((tyco, sorts), (abs, (vs, ty'))) =
      analyze_constructor thy (abs, devarify raw_ty);
    val ty = domain_type ty';
    val (vs', _) = typscheme thy (abs, ty');
  in (tyco, (vs ~~ sorts, ((abs, (vs', ty)), (rep, thm)))) end;


(* code equation certificates *)

fun build_head thy (c, ty) =
  Thm.cterm_of thy (Logic.mk_equals (Free ("HEAD", ty), Const (c, ty)));

fun get_head thy cert_thm =
  let
    val [head] = (#hyps o Thm.crep_thm) cert_thm;
    val (_, Const (c, ty)) = (Logic.dest_equals o Thm.term_of) head;
  in (typscheme thy (c, ty), head) end;

fun typscheme_projection thy =
  typscheme thy o dest_Const o fst o dest_comb o fst o Logic.dest_equals;

fun typscheme_abs thy =
  typscheme thy o dest_Const o fst o strip_comb o snd o dest_comb o fst o Logic.dest_equals o Thm.prop_of;

fun constrain_thm thy vs sorts thm =
  let
    val mapping = map2 (fn (v, sort) => fn sort' =>
      (v, Sorts.inter_sort (Sign.classes_of thy) (sort, sort'))) vs sorts;
    val inst = map2 (fn (v, sort) => fn (_, sort') =>
      (((v, 0), sort), TFree (v, sort'))) vs mapping;
    val subst = (map_types o map_type_tfree)
      (fn (v, _) => TFree (v, the (AList.lookup (op =) mapping v)));
  in
    thm
    |> Thm.varifyT_global
    |> Thm.certify_instantiate (inst, [])
    |> pair subst
  end;

fun concretify_abs thy tyco abs_thm =
  let
    val (_, ((c, _), (_, cert))) = get_abstype_spec thy tyco;
    val lhs = (fst o Logic.dest_equals o Thm.prop_of) abs_thm
    val ty = fastype_of lhs;
    val ty_abs = (fastype_of o snd o dest_comb) lhs;
    val abs = Thm.cterm_of thy (Const (c, ty --> ty_abs));
    val raw_concrete_thm = Drule.transitive_thm OF [Thm.symmetric cert, Thm.combination (Thm.reflexive abs) abs_thm];
  in (c, (Thm.varifyT_global o zero_var_indexes) raw_concrete_thm) end;

fun add_rhss_of_eqn thy t =
  let
    val (args, rhs) = (apfst (snd o strip_comb) o Logic.dest_equals) t;
    fun add_const (Const (c, ty)) = insert (op =) (c, Sign.const_typargs thy (c, ty))
      | add_const _ = I
    val add_consts = fold_aterms add_const
  in add_consts rhs o fold add_consts args end;

val dest_eqn = apfst (snd o strip_comb) o Logic.dest_equals o Logic.unvarify_global;

abstype cert = Nothing of thm
  | Equations of thm * bool list
  | Projection of term * string
  | Abstract of thm * string
with

fun dummy_thm ctxt c =
  let
    val thy = Proof_Context.theory_of ctxt;
    val raw_ty = devarify (const_typ thy c);
    val (vs, _) = typscheme thy (c, raw_ty);
    val sortargs = case Axclass.class_of_param thy c
     of SOME class => [[class]]
      | NONE => (case get_type_of_constr_or_abstr thy c
         of SOME (tyco, _) => (map snd o fst o the)
              (AList.lookup (op =) ((snd o fst o get_type thy) tyco) c)
          | NONE => replicate (length vs) []);
    val the_sort = the o AList.lookup (op =) (map fst vs ~~ sortargs);
    val ty = map_type_tfree (fn (v, _) => TFree (v, the_sort v)) raw_ty
    val chead = build_head thy (c, ty);
  in Thm.weaken chead Drule.dummy_thm end;

fun nothing_cert ctxt c = Nothing (dummy_thm ctxt c);

fun cert_of_eqns ctxt c [] = Equations (dummy_thm ctxt c, [])
  | cert_of_eqns ctxt c raw_eqns = 
      let
        val thy = Proof_Context.theory_of ctxt;
        val eqns = burrow_fst (canonize_thms thy) raw_eqns;
        val _ = map (error_thm (assert_eqn thy) thy) eqns;
        val (thms, propers) = split_list eqns;
        val _ = map (fn thm => if c = const_eqn thy thm then ()
          else error ("Wrong head of code equation,\nexpected constant "
            ^ string_of_const thy c ^ "\n" ^ Display.string_of_thm_global thy thm)) thms;
        fun tvars_of T = rev (Term.add_tvarsT T []);
        val vss = map (tvars_of o snd o head_eqn) thms;
        fun inter_sorts vs =
          fold (curry (Sorts.inter_sort (Sign.classes_of thy)) o snd) vs [];
        val sorts = map_transpose inter_sorts vss;
        val vts = Name.invent_names Name.context Name.aT sorts;
        val thms' =
          map2 (fn vs => Thm.certify_instantiate (vs ~~ map TFree vts, [])) vss thms;
        val head_thm = Thm.symmetric (Thm.assume (build_head thy (head_eqn (hd thms'))));
        fun head_conv ct = if can Thm.dest_comb ct
          then Conv.fun_conv head_conv ct
          else Conv.rewr_conv head_thm ct;
        val rewrite_head = Conv.fconv_rule (Conv.arg1_conv head_conv);
        val cert_thm = Conjunction.intr_balanced (map rewrite_head thms');
      in Equations (cert_thm, propers) end;

fun cert_of_proj thy c tyco =
  let
    val (vs, ((abs, (_, ty)), (rep, _))) = get_abstype_spec thy tyco;
    val _ = if c = rep then () else
      error ("Wrong head of projection,\nexpected constant " ^ string_of_const thy rep);
  in Projection (mk_proj tyco vs ty abs rep, tyco) end;

fun cert_of_abs thy tyco c raw_abs_thm =
  let
    val abs_thm = singleton (canonize_thms thy) raw_abs_thm;
    val _ = assert_abs_eqn thy (SOME tyco) abs_thm;
    val _ = if c = const_abs_eqn thy abs_thm then ()
      else error ("Wrong head of abstract code equation,\nexpected constant "
        ^ string_of_const thy c ^ "\n" ^ Display.string_of_thm_global thy abs_thm);
  in Abstract (Thm.legacy_freezeT abs_thm, tyco) end;

fun constrain_cert_thm thy sorts cert_thm =
  let
    val ((vs, _), head) = get_head thy cert_thm;
    val (subst, cert_thm') = cert_thm
      |> Thm.implies_intr head
      |> constrain_thm thy vs sorts;
    val head' = Thm.term_of head
      |> subst
      |> Thm.cterm_of thy;
    val cert_thm'' = cert_thm'
      |> Thm.elim_implies (Thm.assume head');
  in cert_thm'' end;

fun constrain_cert thy sorts (Nothing cert_thm) =
      Nothing (constrain_cert_thm thy sorts cert_thm)
  | constrain_cert thy sorts (Equations (cert_thm, propers)) =
      Equations (constrain_cert_thm thy sorts cert_thm, propers)
  | constrain_cert thy _ (cert as Projection _) =
      cert
  | constrain_cert thy sorts (Abstract (abs_thm, tyco)) =
      Abstract (snd (constrain_thm thy (fst (typscheme_abs thy abs_thm)) sorts abs_thm), tyco);

fun conclude_cert (Nothing cert_thm) =
      Nothing (Thm.close_derivation cert_thm)
  | conclude_cert (Equations (cert_thm, propers)) =
      Equations (Thm.close_derivation cert_thm, propers)
  | conclude_cert (cert as Projection _) =
      cert
  | conclude_cert (Abstract (abs_thm, tyco)) =
      Abstract (Thm.close_derivation abs_thm, tyco);

fun typscheme_of_cert thy (Nothing cert_thm) =
      fst (get_head thy cert_thm)
  | typscheme_of_cert thy (Equations (cert_thm, _)) =
      fst (get_head thy cert_thm)
  | typscheme_of_cert thy (Projection (proj, _)) =
      typscheme_projection thy proj
  | typscheme_of_cert thy (Abstract (abs_thm, _)) =
      typscheme_abs thy abs_thm;

fun typargs_deps_of_cert thy (Nothing cert_thm) =
      let
        val vs = (fst o fst) (get_head thy cert_thm);
      in (vs, []) end
  | typargs_deps_of_cert thy (Equations (cert_thm, propers)) =
      let
        val vs = (fst o fst) (get_head thy cert_thm);
        val equations = if null propers then [] else
          Thm.prop_of cert_thm
          |> Logic.dest_conjunction_balanced (length propers);
      in (vs, fold (add_rhss_of_eqn thy) equations []) end
  | typargs_deps_of_cert thy (Projection (t, _)) =
      (fst (typscheme_projection thy t), add_rhss_of_eqn thy t [])
  | typargs_deps_of_cert thy (Abstract (abs_thm, tyco)) =
      let
        val vs = fst (typscheme_abs thy abs_thm);
        val (_, concrete_thm) = concretify_abs thy tyco abs_thm;
      in (vs, add_rhss_of_eqn thy (Logic.unvarify_types_global (Thm.prop_of concrete_thm)) []) end;

fun equations_of_cert thy (cert as Nothing _) =
      (typscheme_of_cert thy cert, NONE)
  | equations_of_cert thy (cert as Equations (cert_thm, propers)) =
      let
        val tyscm = typscheme_of_cert thy cert;
        val thms = if null propers then [] else
          cert_thm
          |> Local_Defs.expand [snd (get_head thy cert_thm)]
          |> Thm.varifyT_global
          |> Conjunction.elim_balanced (length propers);
        fun abstractions (args, rhs) = (map (rpair NONE) args, (rhs, NONE));
      in (tyscm, SOME (map (abstractions o dest_eqn o Thm.prop_of) thms ~~ (map SOME thms ~~ propers))) end
  | equations_of_cert thy (Projection (t, tyco)) =
      let
        val (_, ((abs, _), _)) = get_abstype_spec thy tyco;
        val tyscm = typscheme_projection thy t;
        val t' = Logic.varify_types_global t;
        fun abstractions (args, rhs) = (map (rpair (SOME abs)) args, (rhs, NONE));
      in (tyscm, SOME [((abstractions o dest_eqn) t', (NONE, true))]) end
  | equations_of_cert thy (Abstract (abs_thm, tyco)) =
      let
        val tyscm = typscheme_abs thy abs_thm;
        val (abs, concrete_thm) = concretify_abs thy tyco abs_thm;
        fun abstractions (args, rhs) = (map (rpair NONE) args, (rhs, (SOME abs)));
      in
        (tyscm, SOME [((abstractions o dest_eqn o Thm.prop_of) concrete_thm,
          (SOME (Thm.varifyT_global abs_thm), true))])
      end;

fun pretty_cert thy (cert as Nothing _) =
      [Pretty.str "(not implemented)"]
  | pretty_cert thy (cert as Equations _) =
      (map_filter (Option.map (Display.pretty_thm_global thy o Axclass.overload thy) o fst o snd)
         o these o snd o equations_of_cert thy) cert
  | pretty_cert thy (Projection (t, _)) =
      [Syntax.pretty_term_global thy (Logic.varify_types_global t)]
  | pretty_cert thy (Abstract (abs_thm, _)) =
      [(Display.pretty_thm_global thy o Axclass.overload thy o Thm.varifyT_global) abs_thm];

end;


(* code certificate access with preprocessing *)

fun retrieve_raw thy c =
  Symtab.lookup ((the_functions o the_exec) thy) c
  |> Option.map (snd o fst)
  |> the_default None

fun eqn_conv conv ct =
  let
    fun lhs_conv ct = if can Thm.dest_comb ct
      then Conv.combination_conv lhs_conv conv ct
      else Conv.all_conv ct;
  in Conv.combination_conv (Conv.arg_conv lhs_conv) conv ct end;

fun rewrite_eqn conv ctxt =
  singleton (Variable.trade (K (map (Conv.fconv_rule (conv (Simplifier.rewrite ctxt))))) ctxt)

fun preprocess conv ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
  in
    Thm.transfer thy
    #> rewrite_eqn conv ctxt
    #> Axclass.unoverload thy
  end;

fun cert_of_eqns_preprocess ctxt functrans c =
  (perhaps o perhaps_loop o perhaps_apply) functrans
  #> (map o apfst) (preprocess eqn_conv ctxt)
  #> cert_of_eqns ctxt c;

fun get_cert ctxt functrans c =
  let
    val thy = Proof_Context.theory_of ctxt;
  in
    case retrieve_raw thy c of
      Default (_, eqns_lazy) => Lazy.force eqns_lazy
        |> cert_of_eqns_preprocess ctxt functrans c
    | Eqns eqns => eqns
        |> cert_of_eqns_preprocess ctxt functrans c
    | None => nothing_cert ctxt c
    | Proj (_, tyco) => cert_of_proj thy c tyco
    | Abstr (abs_thm, tyco) => abs_thm
       |> preprocess Conv.arg_conv ctxt
       |> cert_of_abs thy tyco c
  end;


(* cases *)

fun case_certificate thm =
  let
    val ((head, raw_case_expr), cases) = (apfst Logic.dest_equals
      o apsnd Logic.dest_conjunctions o Logic.dest_implies o Thm.plain_prop_of) thm;
    val _ = case head of Free _ => true
      | Var _ => true
      | _ => raise TERM ("case_cert", []);
    val ([(case_var, _)], case_expr) = Term.strip_abs_eta 1 raw_case_expr;
    val (Const (case_const, _), raw_params) = strip_comb case_expr;
    val n = find_index (fn Free (v, _) => v = case_var | _ => false) raw_params;
    val _ = if n = ~1 then raise TERM ("case_cert", []) else ();
    val params = map (fst o dest_Var) (nth_drop n raw_params);
    fun dest_case t =
      let
        val (head' $ t_co, rhs) = Logic.dest_equals t;
        val _ = if head' = head then () else raise TERM ("case_cert", []);
        val (Const (co, _), args) = strip_comb t_co;
        val (Var (param, _), args') = strip_comb rhs;
        val _ = if args' = args then () else raise TERM ("case_cert", []);
      in (param, co) end;
    fun analyze_cases cases =
      let
        val co_list = fold (AList.update (op =) o dest_case) cases [];
      in map (AList.lookup (op =) co_list) params end;
    fun analyze_let t =
      let
        val (head' $ arg, Var (param', _) $ arg') = Logic.dest_equals t;
        val _ = if head' = head then () else raise TERM ("case_cert", []);
        val _ = if arg' = arg then () else raise TERM ("case_cert", []);
        val _ = if [param'] = params then () else raise TERM ("case_cert", []);
      in [] end;
    fun analyze (cases as [let_case]) =
          (analyze_cases cases handle Bind => analyze_let let_case)
      | analyze cases = analyze_cases cases;
  in (case_const, (n, analyze cases)) end;

fun case_cert thm = case_certificate thm
  handle Bind => error "bad case certificate"
       | TERM _ => error "bad case certificate";

fun get_case_scheme thy = Option.map fst o Symtab.lookup ((fst o the_cases o the_exec) thy);
fun get_case_cong thy = Option.map snd o Symtab.lookup ((fst o the_cases o the_exec) thy);

val undefineds = Symtab.keys o snd o the_cases o the_exec;


(* diagnostic *)

fun print_codesetup thy =
  let
    val ctxt = Proof_Context.init_global thy;
    val exec = the_exec thy;
    fun pretty_equations const thms =
      (Pretty.block o Pretty.fbreaks)
        (Pretty.str (string_of_const thy const) :: map (Display.pretty_thm_item ctxt) thms);
    fun pretty_function (const, Default (_, eqns_lazy)) =
          pretty_equations const (map fst (Lazy.force eqns_lazy))
      | pretty_function (const, Eqns eqns) = pretty_equations const (map fst eqns)
      | pretty_function (const, None) = pretty_equations const []
      | pretty_function (const, Proj (proj, _)) = Pretty.block
          [Pretty.str (string_of_const thy const), Pretty.fbrk, Syntax.pretty_term ctxt proj]
      | pretty_function (const, Abstr (thm, _)) = pretty_equations const [thm];
    fun pretty_typ (tyco, vs) = Pretty.str
      (string_of_typ thy (Type (tyco, map TFree vs)));
    fun pretty_typspec (typ, (cos, abstract)) = if null cos
      then pretty_typ typ
      else (Pretty.block o Pretty.breaks) (
        pretty_typ typ
        :: Pretty.str "="
        :: (if abstract then [Pretty.str "(abstract)"] else [])
        @ separate (Pretty.str "|") (map (fn (c, (_, [])) => Pretty.str (string_of_const thy c)
             | (c, (_, tys)) =>
                 (Pretty.block o Pretty.breaks)
                    (Pretty.str (string_of_const thy c)
                      :: Pretty.str "of"
                      :: map (Pretty.quote o Syntax.pretty_typ_global thy) tys)) cos)
      );
    fun pretty_caseparam NONE = "<ignored>"
      | pretty_caseparam (SOME c) = string_of_const thy c
    fun pretty_case (const, ((_, (_, [])), _)) = Pretty.str (string_of_const thy const)
      | pretty_case (const, ((_, (_, cos)), _)) = (Pretty.block o Pretty.breaks) [
          Pretty.str (string_of_const thy const), Pretty.str "with",
          (Pretty.block o Pretty.commas o map (Pretty.str o pretty_caseparam)) cos];
    val functions = the_functions exec
      |> Symtab.dest
      |> (map o apsnd) (snd o fst)
      |> sort (string_ord o pairself fst);
    val datatypes = the_types exec
      |> Symtab.dest
      |> map (fn (tyco, (_, (vs, spec)) :: _) =>
          ((tyco, vs), constructors_of spec))
      |> sort (string_ord o pairself (fst o fst));
    val cases = Symtab.dest ((fst o the_cases o the_exec) thy);
    val undefineds = Symtab.keys ((snd o the_cases o the_exec) thy);
  in
    Pretty.writeln_chunks [
      Pretty.block (
        Pretty.str "code equations:" :: Pretty.fbrk
        :: (Pretty.fbreaks o map pretty_function) functions
      ),
      Pretty.block (
        Pretty.str "datatypes:" :: Pretty.fbrk
        :: (Pretty.fbreaks o map pretty_typspec) datatypes
      ),
      Pretty.block (
        Pretty.str "cases:" :: Pretty.fbrk
        :: (Pretty.fbreaks o map pretty_case) cases
      ),
      Pretty.block (
        Pretty.str "undefined:" :: Pretty.fbrk
        :: (Pretty.commas o map (Pretty.str o string_of_const thy)) undefineds
      )
    ]
  end;


(** declaring executable ingredients **)

(* code equations *)

fun gen_add_eqn default (raw_thm, proper) thy =
  let
    val thm = Thm.close_derivation raw_thm;
    val c = const_eqn thy thm;
    fun update_subsume verbose (thm, proper) eqns = 
      let
        val args_of = snd o take_prefix is_Var o rev o snd o strip_comb
          o map_types Type.strip_sorts o fst o Logic.dest_equals o Thm.plain_prop_of;
        val args = args_of thm;
        val incr_idx = Logic.incr_indexes ([], Thm.maxidx_of thm + 1);
        fun matches_args args' =
          let
            val k = length args' - length args
          in if k >= 0
            then Pattern.matchess thy (args, (map incr_idx o drop k) args')
            else false
          end;
        fun drop (thm', proper') = if (proper orelse not proper')
          andalso matches_args (args_of thm') then 
            (if verbose then warning ("Code generator: dropping subsumed code equation\n" ^
                Display.string_of_thm_global thy thm') else (); true)
          else false;
      in (thm, proper) :: filter_out drop eqns end;
    fun natural_order eqns =
      (eqns, Lazy.lazy (fn () => fold (update_subsume false) eqns []))
    fun add_eqn' true (Default (eqns, _)) = Default (natural_order ((thm, proper) :: eqns))
          (*this restores the natural order and drops syntactic redundancies*)
      | add_eqn' true None = Default (natural_order [(thm, proper)])
      | add_eqn' true fun_spec = fun_spec
      | add_eqn' false (Eqns eqns) = Eqns (update_subsume true (thm, proper) eqns)
      | add_eqn' false _ = Eqns [(thm, proper)];
  in change_fun_spec c (add_eqn' default) thy end;

fun gen_add_abs_eqn raw_thm thy =
  let
    val (abs_thm, tyco) = apfst Thm.close_derivation raw_thm;
    val c = const_abs_eqn thy abs_thm;
  in change_fun_spec c (K (Abstr (abs_thm, tyco))) thy end;

fun add_eqn thm thy =
  gen_add_eqn false (mk_eqn thy (thm, true)) thy;

fun add_nbe_eqn thm thy =
  gen_add_eqn false (mk_eqn thy (thm, false)) thy;

fun add_default_eqn thm thy =
  case mk_eqn_liberal thy thm
   of SOME eqn => gen_add_eqn true eqn thy
    | NONE => thy;

val add_default_eqn_attribute = Thm.declaration_attribute
  (fn thm => Context.mapping (add_default_eqn thm) I);
val add_default_eqn_attrib = Attrib.internal (K add_default_eqn_attribute);

fun add_nbe_default_eqn thm thy =
  gen_add_eqn true (mk_eqn thy (thm, false)) thy;

val add_nbe_default_eqn_attribute = Thm.declaration_attribute
  (fn thm => Context.mapping (add_nbe_default_eqn thm) I);
val add_nbe_default_eqn_attrib = Attrib.internal (K add_nbe_default_eqn_attribute);

fun add_abs_eqn raw_thm thy = gen_add_abs_eqn (mk_abs_eqn thy raw_thm) thy;

val add_abs_eqn_attribute = Thm.declaration_attribute
  (fn thm => Context.mapping (add_abs_eqn thm) I);
val add_abs_eqn_attrib = Attrib.internal (K add_abs_eqn_attribute);

fun add_eqn_maybe_abs thm thy =
  case mk_eqn_maybe_abs thy thm
   of SOME (eqn, NONE) => gen_add_eqn false eqn thy
    | SOME ((thm, _), SOME tyco) => gen_add_abs_eqn (thm, tyco) thy
    | NONE => thy;

fun del_eqn thm thy = case mk_eqn_liberal thy thm
 of SOME (thm, _) =>
      let
        fun del_eqn' (Default _) = initial_fun_spec
          | del_eqn' (Eqns eqns) =
              let
                val eqns' = filter_out (fn (thm', _) => Thm.eq_thm_prop (thm, thm')) eqns
              in if null eqns' then None else Eqns eqns' end
          | del_eqn' spec = spec
      in change_fun_spec (const_eqn thy thm) del_eqn' thy end
  | NONE => thy;

fun del_eqns c = change_fun_spec c (K None);

fun del_exception c = change_fun_spec c (K (Eqns []));


(* cases *)

fun case_cong thy case_const (num_args, (pos, _)) =
  let
    val ([x, y], ctxt) = fold_map Name.variant ["A", "A'"] Name.context;
    val (zs, _) = fold_map Name.variant (replicate (num_args - 1) "") ctxt;
    val (ws, vs) = chop pos zs;
    val T = devarify (const_typ thy case_const);
    val Ts = binder_types T;
    val T_cong = nth Ts pos;
    fun mk_prem z = Free (z, T_cong);
    fun mk_concl z = list_comb (Const (case_const, T), map2 (curry Free) (ws @ z :: vs) Ts);
    val (prem, concl) = pairself Logic.mk_equals (pairself mk_prem (x, y), pairself mk_concl (x, y));
  in
    Goal.prove_sorry_global thy (x :: y :: zs) [prem] concl
      (fn {context = ctxt', prems} =>
        Simplifier.rewrite_goals_tac ctxt' prems
        THEN ALLGOALS (Proof_Context.fact_tac ctxt' [Drule.reflexive_thm]))
  end;

fun add_case thm thy =
  let
    val (case_const, (k, cos)) = case_cert thm;
    val _ = case (filter_out (is_constr thy) o map_filter I) cos
     of [] => ()
      | cs => error ("Non-constructor(s) in case certificate: " ^ commas_quote cs);
    val entry = (1 + Int.max (1, length cos), (k, cos));
    fun register_case cong = (map_cases o apfst)
      (Symtab.update (case_const, (entry, cong)));
    fun register_for_constructors (Constructors (cos', cases)) =
         Constructors (cos',
           if exists (fn (co, _) => member (op =) cos (SOME co)) cos'
           then insert (op =) case_const cases
           else cases)
      | register_for_constructors (x as Abstractor _) = x;
    val register_type = (map_typs o Symtab.map)
      (K ((map o apsnd o apsnd) register_for_constructors));
  in
    thy
    |> `(fn thy => case_cong thy case_const entry)
    |-> (fn cong => map_exec_purge (register_case cong #> register_type))
  end;

fun add_undefined c thy =
  (map_exec_purge o map_cases o apsnd) (Symtab.update (c, ())) thy;


(* types *)

fun register_type (tyco, vs_spec) thy =
  let
    val (old_constrs, some_old_proj) =
      case these (Symtab.lookup ((the_types o the_exec) thy) tyco)
       of (_, (_, Constructors (cos, _))) :: _ => (map fst cos, NONE)
        | (_, (_, Abstractor ((co, _), (proj, _)))) :: _ => ([co], SOME proj)
        | [] => ([], NONE);
    val outdated_funs1 = (map fst o fst o constructors_of o snd) vs_spec;
    val outdated_funs2 = case some_old_proj
     of NONE => []
      | SOME old_proj => Symtab.fold
          (fn (c, ((_, spec), _)) =>
            if member (op =) (the_list (associated_abstype spec)) tyco
            then insert (op =) c else I)
            ((the_functions o the_exec) thy) [old_proj];
    fun drop_outdated_cases cases = fold Symtab.delete_safe
      (Symtab.fold (fn (c, ((_, (_, cos)), _)) =>
        if exists (member (op =) old_constrs) (map_filter I cos)
          then insert (op =) c else I) cases []) cases;
  in
    thy
    |> fold del_eqns (outdated_funs1 @ outdated_funs2)
    |> map_exec_purge
        ((map_typs o Symtab.map_default (tyco, [])) (cons (serial (), vs_spec))
        #> (map_cases o apfst) drop_outdated_cases)
  end;

fun unoverload_const_typ thy (c, ty) = (Axclass.unoverload_const thy (c, ty), ty);

structure Datatype_Interpretation =
  Interpretation(type T = string * serial val eq = eq_snd (op =) : T * T -> bool);

fun with_repaired_path f (tyco, serial) thy =
  thy
  |> Sign.root_path
  |> Sign.add_path (Long_Name.qualifier tyco)
  |> f (tyco, serial)
  |> Sign.restore_naming thy;

fun datatype_interpretation f = Datatype_Interpretation.interpretation
  (fn (tyco, _) => fn thy => with_repaired_path f (tyco, fst (get_type thy tyco)) thy);

fun add_datatype proto_constrs thy =
  let
    val constrs = map (unoverload_const_typ thy) proto_constrs;
    val (tyco, (vs, cos)) = constrset_of_consts thy constrs;
  in
    thy
    |> register_type (tyco, (vs, Constructors (cos, [])))
    |> Datatype_Interpretation.data (tyco, serial ())
  end;

fun add_datatype_cmd raw_constrs thy =
  add_datatype (map (read_bare_const thy) raw_constrs) thy;

structure Abstype_Interpretation =
  Interpretation(type T = string * serial val eq = eq_snd (op =) : T * T -> bool);

fun abstype_interpretation f = Abstype_Interpretation.interpretation
  (fn (tyco, _) => fn thy => f (tyco, get_abstype_spec thy tyco) thy);

fun add_abstype proto_thm thy =
  let
    val (tyco, (vs, (abs_ty as (abs, (_, ty)), (rep, cert)))) =
      error_abs_thm (check_abstype_cert thy) thy proto_thm;
  in
    thy
    |> register_type (tyco, (vs, Abstractor (abs_ty, (rep, cert))))
    |> change_fun_spec rep
      (K (Proj (Logic.varify_types_global (mk_proj tyco vs ty abs rep), tyco)))
    |> Abstype_Interpretation.data (tyco, serial ())
  end;


(* setup *)

val _ = Theory.setup
  (let
    fun mk_attribute f = Thm.declaration_attribute (fn thm => Context.mapping (f thm) I);
    fun mk_const_attribute f cs =
      mk_attribute (K (fold (fn c => fn thy => f (read_const thy c) thy) cs));
    val code_attribute_parser =
      Args.$$$ "equation" |-- Scan.succeed (mk_attribute add_eqn)
      || Args.$$$ "nbe" |-- Scan.succeed (mk_attribute add_nbe_eqn)
      || Args.$$$ "abstype" |-- Scan.succeed (mk_attribute add_abstype)
      || Args.$$$ "abstract" |-- Scan.succeed (mk_attribute add_abs_eqn)
      || Args.del |-- Scan.succeed (mk_attribute del_eqn)
      || Args.$$$ "drop" -- Args.colon |-- (Scan.repeat1 Parse.term >> mk_const_attribute del_eqns)
      || Args.$$$ "abort" -- Args.colon |-- (Scan.repeat1 Parse.term >> mk_const_attribute del_exception)
      || Scan.succeed (mk_attribute add_eqn_maybe_abs);
  in
    Datatype_Interpretation.init
    #> Attrib.setup @{binding code} (Scan.lift code_attribute_parser)
        "declare theorems for code generation"
  end);

end; (*struct*)


(* type-safe interfaces for data dependent on executable code *)

functor Code_Data(Data: CODE_DATA_ARGS): CODE_DATA =
struct

type T = Data.T;
exception Data of T;
fun dest (Data x) = x

val kind = Code.declare_data (Data Data.empty);

val data_op = (kind, Data, dest);

fun change_yield (SOME thy) f = Code.change_yield_data data_op thy f
  | change_yield NONE f = f Data.empty

fun change some_thy f = snd (change_yield some_thy (pair () o f));

end;

structure Code : CODE = struct open Code; end;
