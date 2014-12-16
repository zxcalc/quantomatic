(* COMPOUND_RENAMING:
 *
 * A compound renaming applies a renaming to a structure that contains a
 * collection of named items.  The structure needs to have a way of
 * getting the set of existing names, and a function that attempts to
 * rename one of the elements (which is expected to succeed unless there
 * is a clash).
 *
 * Typical usage is to create a renaming that contains some initial constraints
 * (such as a set of names you want to avoid), and pass it to rename to do the
 * actual renaming.
 *
 * mk_renaming will extend an initial renaming to a renaming on the whole
 * collection, which can then be passed do_renaming to actually perform the
 * renaming.  Just using rename is more efficient, however.
 *
 * Due to the way compound renaming works, you need to make sure that the domain
 * and range of nmap are disjoint, and that both are subsets of avoids.
 *)

(* basic things needed for renaming names within another datatype;
   allows separation of name spaces *)
signature COMPOUND_RENAMABLE =
sig
structure Nm : BASIC_NAME
type obj

(* get set of all names *)
val get_nameset : obj -> Nm.NSet.T 

(* rename 1 name *)
val try_rename1 : Nm.name -> Nm.name -> obj -> obj option 
end;

(* IMPROVE:
  ADD: can also make compound renaming from pairwise compound renamings.
  ADD: could also write a version based on a name-mapfold operation on obj
*)


(* generic signature for renaming within a datatype *)
(* NOTE! currently, a renaming may be OK, but because of the order of 
   renamings used, a clash in the obj may be created. e.g. a graph rhs may
   rename a vertex "a" to "b", but "b" may already exists, and have been 
   renamed to "c". While such a renaming is in theory ok, the implementation
   doesn't allow it. What you should be doing is making "b" part of the ignore
   and avoids sets. Thus "a" will be directly renamed to "c", and "b" will be
   ignored.
*)
signature COMPOUND_RENAMING =
sig
type renaming (* see basic renaming for more code to make renamings *)
type obj
(* assumes that existing renaming does not cause clashes,
   preserves non-clashing of renamings *)
val empty_renaming : renaming
val rename : renaming -> obj -> renaming * obj
val mk_renaming : obj -> renaming -> renaming
val do_renaming : renaming -> obj -> obj
(* Note: 
rename rn x = ((mk_renaming x rn), do_renaming (mk_renaming x rn) x)
  (but is more efficient)
*)
(* removes all renaming, but avoids all old names *)
val restrict_to_avoiding : renaming -> renaming

end;


(* construct a compound renaming *)
functor CompoundRenamingFun(CR : COMPOUND_RENAMABLE)
: COMPOUND_RENAMING
= struct
  structure Rnm = CR.Nm.Rnm;
  structure Nm = CR.Nm;

  type renaming = Rnm.T;
  type obj = CR.obj;

  val empty_renaming = Rnm.empty;
  val print_renaming = Rnm.print;

  val restrict_to_avoiding = Rnm.restrict_to_avoiding;

  fun do_renaming rn x = 
      Nm.NSet.fold 
        (fn n => fn x2 =>
            case Rnm.lookup rn n of 
              NONE => x2
            | SOME n2 => (case CR.try_rename1 n n2 x2 
                           of NONE => raise Rnm.invalid_renaming_exp "do_renaming"
                            | SOME x3 => x3 ))
        (CR.get_nameset x) x;

  fun mk_renaming x rn = 
      let val nset = (CR.get_nameset x);
          val avoids = Rnm.get_avoids rn;
          val id_names = Nm.NSet.subtract nset avoids;
          (* first make sure that we set ignore (naming id) on all names not in 
             the avoid set: avoids us creating a name-crossing renaming (where we'd need to do simultainous substitution) *)
          val rn = Rnm.union_ignores id_names rn;
      in 
        Nm.NSet.fold (snd oo Rnm.rename1) (CR.get_nameset x) rn
      end;

(* note: takes time of nset * ln(nset), 
   could be inverted to take time of avoids(rn) * ln(nset) *)
  fun rename rn x = 
      let val nset = (CR.get_nameset x);
          val avoids = Rnm.get_avoids rn;
          val id_names = Nm.NSet.subtract nset avoids;
          (* first make sure that we set ignore (naming id) on all names not in 
             the avoid set: avoids us creating a name-crossing renaming (where we'd need to do simultainous substitution) *)
          val rn = Rnm.union_ignores id_names rn;
      in 
        Nm.NSet.fold 
          (fn n => fn (rn2,x2) =>
              case Rnm.rename1 n rn2 of 
                (NONE, rn3) => (rn3,x2)
              | (SOME n2, rn3) => 
                (case CR.try_rename1 n n2 x2 
                  of NONE => 
                     raise Rnm.invalid_renaming_exp ("compound rename: " ^  
                       (Nm.string_of_name n) ^ " -> " ^ (Nm.string_of_name n2))
                   | SOME x3 => (rn3, x3)))
          nset (rn,x)
      end;
end;


(* IDEA: generalised apply_and_lift: 
   (a -> a * b)                    = rename
   ((a -> a) -> x -> x)            = update
   -> x -> x * b
   This would improve on the get and set code below

   fun lifted_apply producef updatef x = 
       let bref = ref NONE
       in (updatef (fn a => let val (a',b) = producef a;
                               val _ = (bref := SOME b)
                            in a') x, the bref)
       end;

   NOTE: it would take a compiler optimisation to relaise that
         bref always ends up as the b, so you don't need to store the option 
         at all.  
*)

(* IDEA: general notion of embedding one structure within a collection of 
   functions to access to the old data. a Higher-order, polymorphic functor? *)
functor EmbedCompoundRenaming(
  structure Crnm : COMPOUND_RENAMING 
  type T (* Crnm.obj lives inside T *)
  (* try to get internal data, if there is any *)
  val try_to_get : T -> Crnm.obj option
  (* does nothing if it isn't there *)
  val update : (Crnm.obj -> Crnm.obj) -> T -> T 
) : COMPOUND_RENAMING (* where COMPOUND_RENAMING.obj = T *)
= 
struct
  type renaming = Crnm.renaming
  type obj = T
  val empty_renaming = Crnm.empty_renaming;
  fun rename rnm obj = 
      case try_to_get obj of NONE => (rnm,obj)
      | SOME sub_obj => 
        let val (rnm,sub_obj') = Crnm.rename rnm sub_obj
        in (rnm, update (K sub_obj') obj) end;
  fun mk_renaming obj rnm = 
      case try_to_get obj of NONE => rnm
      | SOME sub_obj => Crnm.mk_renaming sub_obj rnm; 
  val do_renaming = update o Crnm.do_renaming;
  val restrict_to_avoiding = Crnm.restrict_to_avoiding;
end;


(* for when obj has no names in it. *)
functor UnitCompoundRenamingFun(type obj)
: COMPOUND_RENAMING
= struct
type renaming = unit;
type obj = obj;
val empty_renaming = ();
fun rename rnm obj = (rnm,obj);
fun mk_renaming obj rnm = rnm;
fun do_renaming rnm obj = obj;
fun restrict_to_avoiding rnm = rnm;
end;


(* for when obj has no names in it. *)
functor EmptyCompoundRenamingFun(structure Nm : BASIC_NAME type obj)
: COMPOUND_RENAMING
= struct
type renaming = Nm.Rnm.T;
type obj = obj;
val empty_renaming = Nm.Rnm.empty;
fun rename rnm obj = (rnm,obj);
fun mk_renaming obj rnm = rnm;
fun do_renaming rnm obj = obj;
fun restrict_to_avoiding rnm = rnm;
end;


(*
signature ALPHA_CONVABLE
= sig 
include COMPOUND_RENAMABLE;

( * fold over two objects as far as they have the same structure 
   assumes f is applied to each pair of names; returns NONE is structure doesn't 
   match.
* )
val fold_2obj_names : (Nm.name * Nm.name -> 'a -> 'a) 
                      -> obj * obj -> 'a -> 'a option
                      
end;


functor AlphaFun(S : ALPHA) 
= struct

exception stop_exp of unit;

fun alpha (objs as (obj1,obj2)) rn = 
    (S.fold_2obj_names 
       (fn (n1,n2) => fn rnm => 
          case S.try_rename1 n1 n2 rnm
            of NONE => raise stop_exp()
             | SOME rnm2 => rnm2) 
       objs
       rn) 
     handle stop_exp _ => NONE;

end;
*)

