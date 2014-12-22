(*  Title:      Pure/Syntax/type_annotation.ML
    Author:     Makarius

Type annotations within syntax trees, notably for pretty printing.
*)

signature TYPE_ANNOTATION =
sig
  val ignore_type: typ -> typ
  val ignore_free_types: term -> term
  val is_ignored: typ -> bool
  val is_omitted: typ -> bool
  val clean: typ -> typ
  val smash: typ -> typ
  val fastype_of: typ list -> term -> typ
end;

structure Type_Annotation: TYPE_ANNOTATION =
struct

(* annotations *)

fun ignore_type T = Type ("_ignore_type", [T]);

val ignore_free_types = Term.map_aterms (fn Free (x, T) => Free (x, ignore_type T) | a => a);

fun is_ignored (Type ("_ignore_type", _)) = true
  | is_ignored _ = false;

fun is_omitted T = is_ignored T orelse T = dummyT;

fun clean (Type ("_ignore_type", [T])) = clean T
  | clean (Type (a, Ts)) = Type (a, map clean Ts)
  | clean T = T;

fun smash (Type ("_ignore_type", [_])) = dummyT
  | smash (Type (a, Ts)) = Type (a, map smash Ts)
  | smash T = T;


(* determine type -- propagate annotations *)

local

fun dest_fun ignored (Type ("fun", [_, T])) = SOME ((ignored ? ignore_type) T)
  | dest_fun _ (Type ("_ignore_type", [T])) = dest_fun true T
  | dest_fun _ _ = NONE;

in

fun fastype_of Ts (t $ u) =
      (case dest_fun false (fastype_of Ts t) of
        SOME T => T
      | NONE => raise TERM ("fastype_of: expected function type", [t $ u]))
  | fastype_of _ (Const (_, T)) = T
  | fastype_of _ (Free (_, T)) = T
  | fastype_of _ (Var (_, T)) = T
  | fastype_of Ts (Bound i) =
      (nth Ts i handle General.Subscript => raise TERM ("fastype_of: Bound", [Bound i]))
  | fastype_of Ts (Abs (_, T, u)) = T --> fastype_of (T :: Ts) u;

end;

end;

