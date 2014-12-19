(*  Title:      Pure/type_infer.ML
    Author:     Stefan Berghofer and Markus Wenzel, TU Muenchen

Basic representation of type-inference problems.
*)

signature TYPE_INFER =
sig
  val is_param: indexname -> bool
  val is_paramT: typ -> bool
  val param_maxidx: term -> int -> int
  val param_maxidx_of: term list -> int
  val param: int -> string * sort -> typ
  val mk_param: int -> sort -> typ
  val anyT: sort -> typ
  val paramify_vars: typ -> typ
  val deref: typ Vartab.table -> typ -> typ
  val finish: Proof.context -> typ Vartab.table -> typ list * term list -> typ list * term list
  val fixate: Proof.context -> term list -> term list
end;

structure Type_Infer: TYPE_INFER =
struct

(** type parameters and constraints **)

(* type inference parameters -- may get instantiated *)

fun is_param (x, _: int) = String.isPrefix "?" x;

fun is_paramT (TVar (xi, _)) = is_param xi
  | is_paramT _ = false;

val param_maxidx =
  (Term.fold_types o Term.fold_atyps)
    (fn (TVar (xi as (_, i), _)) => if is_param xi then Integer.max i else I | _ => I);

fun param_maxidx_of ts = fold param_maxidx ts ~1;

fun param i (x, S) = TVar (("?" ^ x, i), S);

fun mk_param i S = TVar (("?'a", i), S);


(* pre-stage parameters *)

fun anyT S = TFree ("'_dummy_", S);

val paramify_vars =
  Same.commit
    (Term_Subst.map_atypsT_same
      (fn TVar ((x, i), S) => (param i (x, S)) | _ => raise Same.SAME));



(** results **)

(* dereferenced views *)

fun deref tye (T as TVar (xi, _)) =
      (case Vartab.lookup tye xi of
        NONE => T
      | SOME U => deref tye U)
  | deref _ T = T;

fun add_parms tye T =
  (case deref tye T of
    Type (_, Ts) => fold (add_parms tye) Ts
  | TVar (xi, _) => if is_param xi then insert (op =) xi else I
  | _ => I);

fun add_names tye T =
  (case deref tye T of
    Type (_, Ts) => fold (add_names tye) Ts
  | TFree (x, _) => Name.declare x
  | TVar ((x, i), _) => if is_param (x, i) then I else Name.declare x);


(* finish -- standardize remaining parameters *)

fun finish ctxt tye (Ts, ts) =
  let
    val used =
      (fold o fold_types) (add_names tye) ts (fold (add_names tye) Ts (Variable.names_of ctxt));
    val parms = rev ((fold o fold_types) (add_parms tye) ts (fold (add_parms tye) Ts []));
    val names = Name.invent used ("?" ^ Name.aT) (length parms);
    val tab = Vartab.make (parms ~~ names);

    fun finish_typ T =
      (case deref tye T of
        Type (a, Ts) => Type (a, map finish_typ Ts)
      | U as TFree _ => U
      | U as TVar (xi, S) =>
          (case Vartab.lookup tab xi of
            NONE => U
          | SOME a => TVar ((a, 0), S)));
  in (map finish_typ Ts, map (Type.strip_constraints o Term.map_types finish_typ) ts) end;


(* fixate -- introduce fresh type variables *)

fun fixate ctxt ts =
  let
    fun subst_param (xi, S) (inst, used) =
      if is_param xi then
        let
          val [a] = Name.invent used Name.aT 1;
          val used' = Name.declare a used;
        in (((xi, S), TFree (a, S)) :: inst, used') end
      else (inst, used);
    val used = (fold o fold_types) Term.declare_typ_names ts (Variable.names_of ctxt);
    val (inst, _) = fold_rev subst_param (fold Term.add_tvars ts []) ([], used);
  in (map o map_types) (Term_Subst.instantiateT inst) ts end;

end;
