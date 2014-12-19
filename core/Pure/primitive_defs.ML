(*  Title:      Pure/primitive_defs.ML
    Author:     Makarius

Primitive definition forms.
*)

signature PRIMITIVE_DEFS =
sig
  val dest_def: Proof.context -> (term -> bool) -> (string -> bool) -> (string -> bool) ->
    term -> (term * term) * term
  val abs_def: term -> term * term
end;

structure Primitive_Defs: PRIMITIVE_DEFS =
struct

fun term_kind (Const _) = "existing constant "
  | term_kind (Free _) = "free variable "
  | term_kind (Bound _) = "bound variable "
  | term_kind _ = "";

(*c x == t[x] to !!x. c x == t[x]*)
fun dest_def ctxt check_head is_fixed is_fixedT eq =
  let
    fun err msg = raise TERM (msg, [eq]);
    val eq_vars = Term.strip_all_vars eq;
    val eq_body = Term.strip_all_body eq;

    val display_terms =
      commas_quote o map (Syntax.string_of_term ctxt o Syntax_Trans.bound_vars eq_vars);
    val display_types = commas_quote o map (Syntax.string_of_typ ctxt);

    val (raw_lhs, rhs) = Logic.dest_equals eq_body handle TERM _ => err "Not a meta-equality (==)";
    val lhs = Envir.beta_eta_contract raw_lhs;
    val (head, args) = Term.strip_comb lhs;
    val head_tfrees = Term.add_tfrees head [];

    fun check_arg (Bound _) = true
      | check_arg (Free (x, _)) = not (is_fixed x)
      | check_arg (Const ("Pure.type", Type ("itself", [TFree _]))) = true
      | check_arg _ = false;
    fun close_arg (Bound _) t = t
      | close_arg x t = Logic.all x t;

    val lhs_bads = filter_out check_arg args;
    val lhs_dups = duplicates (op aconv) args;
    val rhs_extras = Term.fold_aterms (fn v as Free (x, _) =>
      if is_fixed x orelse member (op aconv) args v then I
      else insert (op aconv) v | _ => I) rhs [];
    val rhs_extrasT = Term.fold_aterms (Term.fold_types (fn v as TFree (a, S) =>
      if is_fixedT a orelse member (op =) head_tfrees (a, S) then I
      else insert (op =) v | _ => I)) rhs [];
  in
    if not (check_head head) then
      err ("Bad head of lhs: " ^ term_kind head ^ display_terms [head])
    else if not (null lhs_bads) then
      err ("Bad arguments on lhs: " ^ display_terms lhs_bads)
    else if not (null lhs_dups) then
      err ("Duplicate arguments on lhs: " ^ display_terms lhs_dups)
    else if not (null rhs_extras) then
      err ("Extra variables on rhs: " ^ display_terms rhs_extras)
    else if not (null rhs_extrasT) then
      err ("Extra type variables on rhs: " ^ display_types rhs_extrasT)
    else if exists_subterm (fn t => t aconv head) rhs then
      err "Entity to be defined occurs on rhs"
    else
      ((lhs, rhs), fold_rev close_arg args (Logic.list_all (eq_vars, (Logic.mk_equals (lhs, rhs)))))
  end;

(*!!x. c x == t[x] to c == %x. t[x]*)
fun abs_def eq =
  let
    val body = Term.strip_all_body eq;
    val vars = map Free (Term.rename_wrt_term body (Term.strip_all_vars eq));
    val (lhs, rhs) = Logic.dest_equals (Term.subst_bounds (vars, body));
    val (lhs', args) = Term.strip_comb lhs;
    val rhs' = fold_rev (absfree o dest_Free) args rhs;
  in (lhs', rhs') end;

end;
