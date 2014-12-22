(*  Title:      Pure/consts.ML
    Author:     Makarius

Polymorphic constants: declarations, abbreviations, additional type
constraints.
*)

signature CONSTS =
sig
  type T
  val eq_consts: T * T -> bool
  val change_base: bool -> T -> T
  val change_ignore: T -> T
  val retrieve_abbrevs: T -> string list -> term -> (term * term) list
  val dest: T ->
   {const_space: Name_Space.T,
    constants: (string * (typ * term option)) list,
    constraints: (string * typ) list}
  val the_const: T -> string -> string * typ                   (*exception TYPE*)
  val the_abbreviation: T -> string -> typ * term              (*exception TYPE*)
  val type_scheme: T -> string -> typ                          (*exception TYPE*)
  val is_monomorphic: T -> string -> bool                      (*exception TYPE*)
  val the_constraint: T -> string -> typ                       (*exception TYPE*)
  val space_of: T -> Name_Space.T
  val alias: Name_Space.naming -> binding -> string -> T -> T
  val is_concealed: T -> string -> bool
  val intern: T -> xstring -> string
  val intern_syntax: T -> xstring -> string
  val check_const: Context.generic -> T -> xstring * Position.T list -> term * Position.report list
  val certify: Context.pretty -> Type.tsig -> bool -> T -> term -> term  (*exception TYPE*)
  val typargs: T -> string * typ -> typ list
  val instance: T -> string * typ list -> typ
  val declare: Context.generic -> binding * typ -> T -> T
  val constrain: string * typ option -> T -> T
  val abbreviate: Context.generic -> Type.tsig -> string -> binding * term -> T -> (term * term) * T
  val revert_abbrev: string -> string -> T -> T
  val hide: bool -> string -> T -> T
  val empty: T
  val merge: T * T -> T
end;

structure Consts: CONSTS =
struct

(** consts type **)

(* datatype T *)

type decl = {T: typ, typargs: int list list};
type abbrev = {rhs: term, normal_rhs: term, force_expand: bool};

datatype T = Consts of
 {decls: (decl * abbrev option) Name_Space.table,
  constraints: typ Symtab.table,
  rev_abbrevs: (term * term) Item_Net.T Symtab.table};

fun eq_consts
   (Consts {decls = decls1, constraints = constraints1, rev_abbrevs = rev_abbrevs1},
    Consts {decls = decls2, constraints = constraints2, rev_abbrevs = rev_abbrevs2}) =
  pointer_eq (decls1, decls2) andalso
  pointer_eq (constraints1, constraints2) andalso
  pointer_eq (rev_abbrevs1, rev_abbrevs2);

fun make_consts (decls, constraints, rev_abbrevs) =
  Consts {decls = decls, constraints = constraints, rev_abbrevs = rev_abbrevs};

fun map_consts f (Consts {decls, constraints, rev_abbrevs}) =
  make_consts (f (decls, constraints, rev_abbrevs));

fun change_base begin = map_consts (fn (decls, constraints, rev_abbrevs) =>
  (Name_Space.change_base begin decls, constraints, rev_abbrevs));

val change_ignore = map_consts (fn (decls, constraints, rev_abbrevs) =>
  (Name_Space.change_ignore decls, constraints, rev_abbrevs));


(* reverted abbrevs *)

val empty_abbrevs =
  Item_Net.init (fn ((t, u), (t', u')) => t aconv t' andalso u aconv u') (single o #1);

fun update_abbrevs mode abbrs =
  Symtab.map_default (mode, empty_abbrevs) (Item_Net.update abbrs);

fun retrieve_abbrevs (Consts {rev_abbrevs, ...}) modes =
  let val nets = map_filter (Symtab.lookup rev_abbrevs) modes
  in fn t => maps (fn net => Item_Net.retrieve net t) nets end;


(* dest consts *)

fun dest (Consts {decls, constraints, ...}) =
 {const_space = Name_Space.space_of_table decls,
  constants =
    Name_Space.fold_table (fn (c, ({T, ...}, abbr)) =>
      cons (c, (T, Option.map #rhs abbr))) decls [],
  constraints = Symtab.dest constraints};


(* lookup consts *)

fun the_entry (Consts {decls, ...}) c =
  (case Name_Space.lookup_key decls c of
    SOME entry => entry
  | NONE => raise TYPE ("Unknown constant: " ^ quote c, [], []));

fun the_const consts c =
  (case the_entry consts c of
    (c', ({T, ...}, NONE)) => (c', T)
  | _ => raise TYPE ("Not a logical constant: " ^ quote c, [], []));

fun the_abbreviation consts c =
  (case the_entry consts c of
    (_, ({T, ...}, SOME {rhs, ...})) => (T, rhs)
  | _ => raise TYPE ("Not an abbreviated constant: " ^ quote c, [], []));

fun the_decl consts = #1 o #2 o the_entry consts;
val type_scheme = #T oo the_decl;
val type_arguments = #typargs oo the_decl;

val is_monomorphic = null oo type_arguments;

fun the_constraint (consts as Consts {constraints, ...}) c =
  (case Symtab.lookup constraints c of
    SOME T => T
  | NONE => type_scheme consts c);


(* name space and syntax *)

fun space_of (Consts {decls, ...}) = Name_Space.space_of_table decls;

fun alias naming binding name = map_consts (fn (decls, constraints, rev_abbrevs) =>
  ((Name_Space.alias_table naming binding name decls), constraints, rev_abbrevs));

val is_concealed = Name_Space.is_concealed o space_of;

val intern = Name_Space.intern o space_of;

fun intern_syntax consts s =
  (case try Lexicon.unmark_const s of
    SOME c => c
  | NONE => intern consts s);


(* check_const *)

fun check_const context consts (xname, ps) =
  let
    val Consts {decls, ...} = consts;
    val ((c, reports), _) = Name_Space.check_reports context decls (xname, ps);
    val T = type_scheme consts c handle TYPE (msg, _, _) => error (msg ^ Position.here_list ps);
  in (Const (c, T), reports) end;


(* certify *)

fun certify pp tsig do_expand consts =
  let
    fun err msg (c, T) =
      raise TYPE (msg ^ " " ^ quote c ^ " :: " ^
        Syntax.string_of_typ (Syntax.init_pretty pp) T, [], []);
    val certT = Type.cert_typ tsig;
    fun cert tm =
      let
        val (head, args) = Term.strip_comb tm;
        val args' = map cert args;
        fun comb head' = Term.list_comb (head', args');
      in
        (case head of
          Abs (x, T, t) => comb (Abs (x, certT T, cert t))
        | Const (c, T) =>
            let
              val T' = certT T;
              val (_, ({T = U, ...}, abbr)) = the_entry consts c;
              fun expand u =
                Term.betapplys (Envir.expand_atom T' (U, u) handle TYPE _ =>
                  err "Illegal type for abbreviation" (c, T), args');
            in
              if not (Type.raw_instance (T', U)) then
                err "Illegal type for constant" (c, T)
              else
                (case abbr of
                  SOME {rhs, normal_rhs, force_expand} =>
                    if do_expand then expand normal_rhs
                    else if force_expand then expand rhs
                    else comb head
                | _ => comb head)
            end
        | _ => comb head)
      end;
  in cert end;


(* typargs -- view actual const type as instance of declaration *)

local

fun args_of (Type (_, Ts)) pos = args_of_list Ts 0 pos
  | args_of (TVar v) pos = insert (eq_fst op =) (v, rev pos)
  | args_of (TFree _) _ = I
and args_of_list (T :: Ts) i is = args_of T (i :: is) #> args_of_list Ts (i + 1) is
  | args_of_list [] _ _ = I;

fun subscript (Type (_, Ts)) (i :: is) = subscript (nth Ts i) is
  | subscript T [] = T
  | subscript _ _ = raise Subscript;

in

fun typargs_of T = map #2 (rev (args_of T [] []));

fun typargs consts (c, T) = map (subscript T) (type_arguments consts c);

end;

fun instance consts (c, Ts) =
  let
    val declT = type_scheme consts c;
    val vars = map Term.dest_TVar (typargs consts (c, declT));
    val inst = vars ~~ Ts handle ListPair.UnequalLengths =>
      raise TYPE ("Consts.instance", Ts, [Const (c, dummyT)]);
  in declT |> Term_Subst.instantiateT inst end;



(** build consts **)

(* name space *)

fun hide fully c = map_consts (fn (decls, constraints, rev_abbrevs) =>
  (Name_Space.hide_table fully c decls, constraints, rev_abbrevs));


(* declarations *)

fun declare context (b, declT) =
  map_consts (fn (decls, constraints, rev_abbrevs) =>
    let
      val decl = {T = declT, typargs = typargs_of declT};
      val _ = Binding.check b;
      val (_, decls') = decls |> Name_Space.define context true (b, (decl, NONE));
    in (decls', constraints, rev_abbrevs) end);


(* constraints *)

fun constrain (c, C) consts =
  consts |> map_consts (fn (decls, constraints, rev_abbrevs) =>
    (#2 (the_entry consts c) handle TYPE (msg, _, _) => error msg;
      (decls,
        constraints |> (case C of SOME T => Symtab.update (c, T) | NONE => Symtab.delete_safe c),
        rev_abbrevs)));


(* abbreviations *)

local

fun strip_abss (t as Abs (x, T, b)) =
      if Term.is_dependent b then strip_abss b |>> cons (x, T)  (* FIXME decr!? *)
      else ([], t)
  | strip_abss t = ([], t);

fun rev_abbrev lhs rhs =
  let
    val (xs, body) = strip_abss (Envir.beta_eta_contract rhs);
    val vars = fold (fn (x, T) => cons (Var ((x, 0), T))) (Term.rename_wrt_term body xs) [];
  in (Term.subst_bounds (rev vars, body), Term.list_comb (lhs, vars)) end;

in

fun abbreviate context tsig mode (b, raw_rhs) consts =
  let
    val pp = Context.pretty_generic context;
    val cert_term = certify pp tsig false consts;
    val expand_term = certify pp tsig true consts;
    val force_expand = mode = Print_Mode.internal;

    val _ = Term.exists_subterm Term.is_Var raw_rhs andalso
      error ("Illegal schematic variables on rhs of abbreviation " ^ Binding.print b);

    val rhs = raw_rhs
      |> Term.map_types (Type.cert_typ tsig)
      |> cert_term
      |> Term.close_schematic_term;
    val normal_rhs = expand_term rhs;
    val T = Term.fastype_of rhs;
    val lhs = Const (Name_Space.full_name (Name_Space.naming_of context) b, T);
  in
    consts |> map_consts (fn (decls, constraints, rev_abbrevs) =>
      let
        val decl = {T = T, typargs = typargs_of T};
        val abbr = {rhs = rhs, normal_rhs = normal_rhs, force_expand = force_expand};
        val _ = Binding.check b;
        val (_, decls') = decls
          |> Name_Space.define context true (b, (decl, SOME abbr));
        val rev_abbrevs' = rev_abbrevs
          |> update_abbrevs mode (rev_abbrev lhs rhs);
      in (decls', constraints, rev_abbrevs') end)
    |> pair (lhs, rhs)
  end;

fun revert_abbrev mode c consts = consts |> map_consts (fn (decls, constraints, rev_abbrevs) =>
  let
    val (T, rhs) = the_abbreviation consts c;
    val rev_abbrevs' = rev_abbrevs
      |> update_abbrevs mode (rev_abbrev (Const (c, T)) rhs);
  in (decls, constraints, rev_abbrevs') end);

end;


(* empty and merge *)

val empty =
  make_consts (Name_Space.empty_table Markup.constantN, Symtab.empty, Symtab.empty);

fun merge
   (Consts {decls = decls1, constraints = constraints1, rev_abbrevs = rev_abbrevs1},
    Consts {decls = decls2, constraints = constraints2, rev_abbrevs = rev_abbrevs2}) =
  let
    val decls' = Name_Space.merge_tables (decls1, decls2);
    val constraints' = Symtab.merge (K true) (constraints1, constraints2);
    val rev_abbrevs' = Symtab.join (K Item_Net.merge) (rev_abbrevs1, rev_abbrevs2);
  in make_consts (decls', constraints', rev_abbrevs') end;

end;
