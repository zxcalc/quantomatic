(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      names_map.ML                                           
    Author:     Lucas Dixon - lucas.dixon@ed.ac.uk                     *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

    Name mappings: (not-necessarily-isomorphic) functions over
    finite sets of names

    Note: not necessarily surjective (inverse set is not always
    defined), and may not be injective either.

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_MAP_SHARING = (* all types for sharing *)
sig
  structure Dom : NAME_SHARING
  structure Cod : NAME_SHARING
  type T
end;
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
signature NAME_MAP = 
sig 
structure Dom : NAME (* domain elements *)
structure Cod : NAME (* codomain elements *)

(* name map type *)
type T

val empty : T
val is_empty : T -> bool

(* add?: union, intersection, ... *)

(* contains/lookup (raised Tab.UNDEF exception if dom does not exist) *)
val assert_contains : T -> (Dom.name * Cod.name) -> bool
(* lookup: NONE if domain element does not exist *)
val lookup : T -> (Dom.name * Cod.name) -> bool option

(* domain and codomain functions *)
exception not_in_dom_exp of Dom.name * T;
exception not_in_cod_exp of Cod.name * T;
val domf : T -> Dom.name -> Cod.name   (* raise *)
val codf : T -> Cod.name -> Dom.NSet.T (* raise *)

(* internal; partial function, NONE when there is no such set *)
val lookup_domf : T -> Dom.name -> Cod.name option
val lookup_codf : T -> Cod.name -> Dom.NSet.T option
val dom_contains : T -> Dom.name -> bool
val cod_contains : T -> Cod.name -> bool

(* add and delete many things *)
(* val add_many_to_1cod : Dom.NSet.T -> Cod.name -> T -> T *)
(* val del_many_from_1cod : Cod.name -> Dom.NSet.T -> T -> T *)

(* remove things preserving surjectivity: delete unmapped cod elements *)
val surj_del_dom' : Dom.name -> T 
                   -> (Cod.name option (* removed this from cod *)
                       * T)
val surj_del_dom : Dom.name -> T -> T
(* basic delete: leaves unampped elements in the map *)
val del_dom : Dom.name -> T -> T
val del_cod : Cod.name -> T -> T

(* adding mappings *)
val add_to_dom : Dom.name -> Cod.name -> T -> T
val add_to_cod : Cod.name -> Dom.NSet.T -> T -> T

(* extend/update existing map with *)
val extend_cod_by1 : Dom.name -> Cod.name -> T -> T
val extend_cod_by : Dom.NSet.T -> Cod.name -> T -> T

(* take union of two maps; assumes disjoint domains *)
val union_merge : T -> T -> T

(* renaming *)
val rename1_dom : Dom.name -> Dom.name -> T -> (Dom.name * T)
val rename1_cod : Cod.name -> Cod.name -> T -> (Cod.name * T)

val rename1_opt_dom : Dom.name -> Dom.name -> T -> (Dom.name option * T)
val rename1_opt_cod : Cod.name -> Cod.name -> T -> (Cod.name option * T)

val try_rename1_dom : Dom.name -> Dom.name -> T -> T option
val try_rename1_cod : Cod.name -> Cod.name -> T -> T option

(* converting to/from lists *)
val of_list : (Dom.name * Cod.name) list -> T
val list_of : T -> (Dom.name * Cod.name) list

val of_codf_list : (Cod.name * Dom.NSet.T) list -> T
val codf_list_of : T -> (Cod.name * Dom.NSet.T) list

(* get internal tables *)
val get_domset : T -> Dom.NSet.T
val get_codset : T -> Cod.NSet.T
val get_domlist : T -> Dom.name list
val get_codlist : T -> Cod.name list
val get_domtab : T -> Cod.name Dom.NTab.T
val get_codtab : T -> Dom.NSet.T Cod.NTab.T

(* restrict to a given domain/codomain *)
val restrict_dom_to : Dom.NSet.T -> T -> T
val restrict_cod_to : Cod.NSet.T -> T -> T

val premap : {ignore : Dom.NSet.T,
              avoids : Dom.NSet.T,
              domf : Dom.name Dom.NTab.T,
              codf : Dom.name Dom.NTab.T } -> T -> T

(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit

structure Sharing : NAME_MAP_SHARING
sharing Sharing.Dom = Dom.Sharing
sharing Sharing.Cod = Cod.Sharing
sharing type Sharing.T = T;

end;

(* *)
functor NameMapFun(structure Dom : NAME and Cod : NAME) 
: NAME_MAP
= struct 

structure Dom = Dom; (* argument 1 name type *)
structure Cod = Cod; (* argument 2 name type *)

(* datatype for meta variable environments *)
(* invarient: *)
datatype T = NameMap of {
         domf : Cod.name Dom.NTab.T,
         codf : Dom.NSet.T Cod.NTab.T
};

val empty = NameMap {domf = Dom.NTab.empty,
                     codf = Cod.NTab.empty};

fun get_domtab (NameMap rep) = #domf rep;
fun get_codtab (NameMap rep) = #codf rep;

val get_domset = Dom.NTab.get_nameset o get_domtab;
val get_codset = Cod.NTab.get_nameset o get_codtab;

val is_empty = Dom.NTab.is_element_empty o get_domtab;

val get_domlist = Dom.NSet.list_of o get_domset;
val get_codlist = Cod.NSet.list_of o get_codset;

fun update_domf f (NameMap rep) = 
    NameMap {domf = f (#domf rep), codf = (#codf rep)}

fun update_codf f (NameMap rep) = 
    NameMap {domf = (#domf rep), codf = f (#codf rep)}

val set_domtab = update_domf o K;
val set_codtab = update_codf o K;

fun union_merge f1 f2 = 
    let 
      fun merge_codfs c1 c2 =
      Cod.NTab.fold
        (fn (v as (n,a)) => fn c3 => 
            case Cod.NTab.lookup c3 n
             of NONE => Cod.NTab.update_new (n,a) c3
              | SOME a2 => Cod.NTab.update (n,Dom.NSet.union_merge a a2) c3)
        c1 c2;
    in
      NameMap { 
        domf = Dom.NTab.merge_joint 
           Cod.name_eq (get_domtab f1) (get_domtab f2),
        codf = merge_codfs (get_codtab f1) (get_codtab f2)
      }
    end;


(* implicit: m n *)
val lookup_domf = Dom.NTab.lookup o get_domtab;
val lookup_codf = Cod.NTab.lookup o get_codtab;

val dom_contains = Dom.NTab.contains_name o get_domtab;
val cod_contains = Cod.NTab.contains_name o get_codtab;


exception not_in_dom_exp of Dom.name * T;
exception not_in_cod_exp of Cod.name * T;

fun domf m n = 
    case lookup_domf m n of 
      NONE => raise not_in_dom_exp (n,m)
    | SOME x => x;

fun codf m n = 
    case lookup_codf m n of 
      NONE => raise not_in_cod_exp (n,m) 
    | SOME x => x;

(* given two elements check if function maps them *)
fun assert_contains m (a,b) = Cod.name_eq (domf m a, b);
fun lookup m (a,b) = 
    (case lookup_domf m a 
      of NONE => NONE
       | SOME b2 => SOME (Cod.name_eq (b2,b)));
fun contains m a_b = 
    (case lookup m a_b of SOME true => true | _ => false);

(*  aset cannot be mapped already *)
fun add_many_to_1cod aset b m = 
    m |> update_domf (Dom.NSet.fold
                        (fn a => Dom.NTab.ins (a,b)) 
                        aset)
      |> update_codf (fn batab2 => 
           case Cod.NTab.lookup batab2 b of
             NONE => Cod.NTab.ins (b,aset) batab2
           | SOME oldaset => 
             Cod.NTab.update (b,Dom.NSet.union_merge aset oldaset) batab2
           );

(*  note: remaps aset if previously mapped *)
fun extend_cod_by aset b m = 
    let val aset0 = case lookup_codf m b of NONE => Dom.NSet.empty
                                          | SOME aset0 => aset0
    in
      m |> update_domf (Dom.NSet.fold
                          (fn a => Dom.NTab.update (a,b)) aset)
        |> update_codf (Cod.NTab.update (b,Dom.NSet.union_merge aset aset0))
    end;

val extend_cod_by1 = extend_cod_by o Dom.NSet.single;



(* delete many elements *)
fun surj_del_dom' a m = 
    (case lookup_domf m a of 
       NONE => (NONE,m)
     | SOME b => 
       let val aset = Dom.NSet.delete a (codf m b)           
           val codtab = get_codtab m
           val (delopt, codtab2) = 
               (* surjective delete:, if unmapped cod, remove from cod *)
               if Dom.NSet.is_empty aset then 
                 (SOME b, Cod.NTab.delete b codtab)
               else
               (NONE, Cod.NTab.update (b,aset) codtab)
       in
         (delopt, 
          m |> set_codtab codtab2
            |> update_domf (Dom.NTab.delete a))
       end);
val surj_del_dom = snd oo surj_del_dom';

(* non-surjective delete: leaves unampped elements in the codomain *)
fun del_dom' a m = 
    (case lookup_domf m a of 
       NONE => m
     | SOME b => 
       m |> update_codf (Cod.NTab.map_entry (Dom.NSet.delete a) b)
         |> update_domf (Dom.NTab.delete a));
(* also removes domain elements that map to it *)
fun del_cod' b m = 
    (case lookup_codf m b of 
       NONE => (Dom.NSet.empty, m)
     | SOME aset => 
       (aset, m |> update_codf (Cod.NTab.delete b)
                |> update_domf (Dom.NSet.fold Dom.NTab.delete aset)));

(* deleting all entries *)
val del_dom = del_dom';
val del_cod = snd oo del_cod';

(* implicit: a b m *)
val add_to_dom = add_many_to_1cod o Dom.NSet.single;
(* implicit: m *)
fun add_to_cod b aset = add_many_to_1cod aset b;

(* implicit: a b m *)
val add1 = add_to_dom;
(* implicit: a m *)
(* val del1_from_cod = del_dom; *)

(* convert to/from lists *)
val list_of = Dom.NTab.list_of o get_domtab;
fun of_list l = fold (fn (a,b) => add1 a b) l empty;

val codf_list_of = Cod.NTab.list_of o get_codtab;
fun of_codf_list l = fold (fn (b,aset) => add_to_cod b aset) l empty;

(* renaming *)

(* rename domain element *)    
fun rename1_opt_dom a1 a2 m = 
    let val domtab = get_domtab m
    in 
      case Dom.NTab.lookup domtab a1 of 
        NONE => (NONE, m)
      | SOME b => 
        let val (a3opt,domtab2) = Dom.NTab.rename1_opt a1 a2 domtab
            val a3 = case a3opt of NONE => a2 | SOME a3' => a3'
        in 
          (a3opt,
           m |> set_domtab domtab2
             |> update_codf 
             (Cod.NTab.map_entry (snd o Dom.NSet.rename1 a1 a3) b))
        end
    end;

fun rename1_dom a1 a2 m = 
    let val domtab = get_domtab m
    in 
      case Dom.NTab.lookup domtab a1 of 
        NONE => (a2, m)
      | SOME b => 
        let val (a3,domtab2) = Dom.NTab.rename1 a1 a2 domtab
        in 
          (a3,
           m |> set_domtab domtab2
             |> update_codf 
             (Cod.NTab.map_entry (snd o Dom.NSet.rename1 a1 a3) b))
        end
    end;

(* NONE on name clash *)
fun try_rename1_dom a1 a2 m = 
    let val domtab = get_domtab m
    in case Dom.NTab.try_rename1 a1 a2 domtab of (* try to update domain *)
         NONE => NONE (* name clash *)
       | SOME domtab2 => 
         (case Dom.NTab.lookup domtab a1 of 
            NONE => SOME m (* nothing in codomain *)
          | SOME b => 
            SOME (m |> set_domtab domtab2
                    |> update_codf (* update codomain also *)
                        (Cod.NTab.map_entry (snd o Dom.NSet.rename1 a1 a2) b)))
    end;

(* rename codomain element *)    
fun rename1_opt_cod a1 a2 m = 
    let val codtab = get_codtab m
    in 
      case Cod.NTab.lookup codtab a1 of 
        NONE => (NONE, m)
      | SOME domset => 
        let val (a3opt,codtab2) = Cod.NTab.rename1_opt a1 a2 codtab
            val a3 = case a3opt of NONE => a2 | SOME a3' => a3'
        in 
          (a3opt,
           m |> set_codtab codtab2
               |> update_domf 
               (fn domtab => 
                   Dom.NSet.fold (Dom.NTab.map_entry (K a3))
                                 domset domtab))
        end
    end;

fun rename1_cod a1 a2 m = 
    let val codtab = get_codtab m
    in 
      case Cod.NTab.lookup codtab a1 of 
        NONE => (a2, m)
      | SOME domset => 
        let val (a3,codtab2) = Cod.NTab.rename1 a1 a2 codtab in 
          (a3,
           m |> set_codtab codtab2
               |> update_domf 
               (fn domtab => 
                   Dom.NSet.fold (Dom.NTab.map_entry (K a3))
                                 domset domtab))
        end
    end;

fun try_rename1_cod a1 a2 m = 
    let val codtab = get_codtab m
    in case Cod.NTab.try_rename1 a1 a2 codtab of 
         NONE => NONE
       | SOME codtab2 => 
         (case Cod.NTab.lookup codtab a1 of 
            NONE => SOME m
          | SOME domset => 
           SOME (m |> set_codtab codtab2
                   |> update_domf 
                   (fn domtab => 
                       Dom.NSet.fold (Dom.NTab.map_entry (K a2))
                                     domset domtab)))
    end;

(* takes time prop to dom *)
fun restrict_dom_to vs m = 
    Dom.NSet.fold 
      (fn d => fn m2 => if Dom.NSet.contains vs d then m2 
        else del_dom d m2)
      (get_domset m)
      m;

fun restrict_cod_to vs m = 
    Cod.NSet.fold 
      (fn n => fn m2 => if Cod.NSet.contains vs n then m2 
        else del_cod n m2)
      (get_codset m)
      m;

(* EXTREME HACKING. We need some nice, unified way of combining maps. *)
fun premap a (NameMap b) =
    NameMap ({domf = Dom.NTab.map_all (K (domf (NameMap b))) (#domf a),
              codf = #codf b });

(* pretty printing *)
fun pretty m = 
    Pretty.chunks 
    [Pretty.str "NameMap{",
     Pretty.block [Pretty.str "Dom->Cod:",  
                   Dom.NTab.pretty Cod.pretty_name (get_domtab m)],
     Pretty.block [Pretty.str "Cod->Dom:",  
                   Cod.NTab.pretty Dom.NSet.pretty (get_codtab m)],
     Pretty.str "}"];
val print = Pretty.writeln o pretty;

(* *)
structure Sharing = 
struct
  structure Dom = Dom.Sharing
  structure Cod = Cod.Sharing
  type T = T
end;

end;
