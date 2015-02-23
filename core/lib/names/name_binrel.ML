(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      name_binrel.ML
    Author:     Lucas Dixon, University of Edinburgh
                lucas.dixon@ed.ac.uk
    Updated:    10 Dec 2008
    Date:       10 Dec 2008
*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  
    Binary Relations over finite name sets 
    (objects with order and fresh construction operations)
    (e.g. two way linked dependencies on names)

    Curretly removes element from dom if there is nothing in cod to 
    related to it; and visa-versa for cod/dom. 

    POSSIBLE IMPROVEMENT: lazy updates? allow unrelated elements? 
*)
signature NAME_BINREL_SHARING
= sig
structure Dom : NAME_SHARING
structure Cod : NAME_SHARING
type T
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
signature NAME_BINREL 
= sig

structure Dom : NAME (* domain elements *)
structure Cod : NAME (* codomain elements *)
type T

val empty : T
val is_empty : T -> bool

(* add?: union, intersection, ... *)

(* is the pair in the relation? *)
val contains : T -> (Dom.name * Cod.name) -> bool 
(* lookup: gives NONE if the domain is not even in the relation *)
val lookup : T -> (Dom.name * Cod.name) -> bool option

(* getting related sets of names (domain and codomain functions) *)
val domf : T -> Dom.name -> Cod.NSet.T (* is empty if none *)
val codf : T -> Cod.name -> Dom.NSet.T (* is empty if none *)

(* internal; partial function, NONE when there is no such set *)
val lookup_domf : T -> Dom.name -> Cod.NSet.T option
val lookup_codf : T -> Cod.name -> Dom.NSet.T option

(* add and delete many binary relations; 
   all deletes also remove disconnected elements *)
val add_many : Dom.NSet.T -> Cod.NSet.T -> T -> T
val del_many : Dom.NSet.T -> Cod.NSet.T -> T -> T

(* 1 to many relations *)
val add_to_dom : Dom.name -> Cod.NSet.T -> T -> T
val add_to_cod : Cod.name -> Dom.NSet.T -> T -> T

val del_from_dom : Dom.name -> Cod.NSet.T -> T -> T
val del_from_cod : Cod.name -> Dom.NSet.T -> T -> T

val del_dom : Dom.name -> T -> T
val del_cod : Cod.name -> T -> T

(* 1-1 relations, union/subtraction (do not raise) *)
val add1 : Dom.name -> Cod.name -> T -> T
val del1 : Dom.name -> Cod.name -> T -> T

(* union of two relations *)
val union_merge : T -> T -> T

(* rename *)
val rename1_dom : Dom.name -> Dom.name -> T -> (Dom.name * T)
val rename1_cod : Cod.name -> Cod.name -> T -> (Cod.name * T)

val rename1_opt_dom : Dom.name -> Dom.name -> T -> (Dom.name option * T)
val rename1_opt_cod : Cod.name -> Cod.name -> T -> (Cod.name option * T)

val try_rename1_dom : Dom.name -> Dom.name -> T -> T option
val try_rename1_cod : Cod.name -> Cod.name -> T -> T option

(* converting to/from lists *)
val of_rawlist : (Dom.name * Cod.name) list -> T
val rawlist_of : T -> (Dom.name * Cod.name) list

val of_list : (Dom.name * Cod.NSet.T) list -> T
val list_of : T -> (Dom.name * Cod.NSet.T) list

(* get internal tables *)
val get_domset : T -> Dom.NSet.T
val get_codset : T -> Cod.NSet.T
val get_domtab : T -> Cod.NSet.T Dom.NTab.T
val get_codtab : T -> Dom.NSet.T Cod.NTab.T


(* pretty printing *)
val pretty : T -> Pretty.T
val print : T -> unit

(* sharing substruct *)
structure Sharing : NAME_BINREL_SHARING
sharing Sharing.Dom = Dom.Sharing
sharing Sharing.Cod = Cod.Sharing
sharing type Sharing.T = T;

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
functor NameBRelFun(structure Dom : NAME and Cod : NAME) 
: NAME_BINREL
= struct

structure Dom = Dom; (* argument 1 name type *)
structure Cod = Cod; (* argument 2 name type *)

(* datatype for meta variable environments *)
(* invarient: *)
datatype T = BinRel of {
         domf : Cod.NSet.T Dom.NTab.T,
         codf : Dom.NSet.T Cod.NTab.T
};

fun get_domtab (BinRel rep) = #domf rep;
fun get_codtab (BinRel rep) = #codf rep;

val empty = BinRel {domf = Dom.NTab.empty,
                    codf = Cod.NTab.empty};

val is_empty = Dom.NTab.is_element_empty o get_domtab;

val get_domset = Dom.NTab.get_nameset o get_domtab;
val get_codset = Cod.NTab.get_nameset o get_codtab;

fun update_domf f (BinRel rep) = 
    BinRel {domf = f (#domf rep), codf = (#codf rep)}

fun update_codf f (BinRel rep) = 
    BinRel {domf = (#domf rep), codf = f (#codf rep)}

val set_domtab = update_domf o K;
val set_codtab = update_codf o K;

(* implicit: rel n *)
val lookup_domf = Dom.NTab.lookup o get_domtab;
val lookup_codf = Cod.NTab.lookup o get_codtab;

fun domf rel n = 
    case lookup_domf rel n of NONE => Cod.NSet.empty | SOME x => x;

fun codf rel n = 
    case lookup_codf rel n of NONE => Dom.NSet.empty | SOME x => x;

(* given two elements check if bin-rel holds for them *)
fun contains rel (a,b) = Cod.NSet.contains (domf rel a) b;
fun lookup rel (a,b) = 
    (case lookup_domf rel a 
      of NONE => NONE
       | SOME bset => SOME (Cod.NSet.contains bset b));


(* add and remove things from the binary relation. *)
fun add_many aset bset rel = 
    if (Dom.NSet.is_empty aset) orelse (Cod.NSet.is_empty bset) then rel
    else
    rel |> update_domf 
        (fn abtab => 
            Dom.NSet.fold
              (fn a => fn abtab2 => 
                  case Dom.NTab.lookup abtab2 a of
                    NONE => Dom.NTab.ins (a,bset) abtab2
                  | SOME oldbset => Dom.NTab.update (a,Cod.NSet.union_merge bset oldbset) abtab2
              ) aset abtab)
        |> update_codf
        (fn batab => 
            Cod.NSet.fold
              (fn b => fn batab2 => 
                  case Cod.NTab.lookup batab2 b of
                    NONE => Cod.NTab.ins (b,aset) batab2
                  | SOME oldaset => Cod.NTab.update (b,Dom.NSet.union_merge aset oldaset) batab2
              ) bset batab);

fun del_many aset bset rel = 
    rel |> update_domf 
        (fn abtab => 
            Dom.NSet.fold
              (fn a => fn abtab2 => 
                  case Dom.NTab.lookup abtab2 a of
                    NONE => abtab2
                  | SOME oldbset => 
                    let val newbset = Cod.NSet.subtract oldbset bset 
                    in if Cod.NSet.is_empty newbset then Dom.NTab.delete a abtab2
                       else Dom.NTab.update (a,newbset) abtab2 end)
              aset abtab)
        |> update_codf
        (fn batab => 
            Cod.NSet.fold
              (fn b => fn batab2 => 
                  case Cod.NTab.lookup batab2 b of
                    NONE => batab2
                  | SOME oldaset => 
                    let val newaset = Dom.NSet.subtract oldaset aset 
                    in if Dom.NSet.is_empty newaset then Cod.NTab.delete b batab2
                       else Cod.NTab.update (b,newaset) batab2 end)
              bset batab);

(* 1-many relations *)
(* implicit: rel *)
fun add_to_dom a bset = add_many (Dom.NSet.single a) bset;
fun add_to_cod b aset = add_many aset (Cod.NSet.single b);
fun del_from_dom a bset = del_many (Dom.NSet.single a) bset;
fun del_from_cod b aset = del_many aset (Cod.NSet.single b);

(* 1-1 relations *)
(* implicit: rel *)
fun add1 a b = add_many (Dom.NSet.single a) (Cod.NSet.single b);
fun del1 a b = del_many (Dom.NSet.single a) (Cod.NSet.single b);

(* union of two binary relations *)
fun union_merge r1 r2 = 
  r2 
  |> Dom.NTab.fold (fn (a,bset) => add_to_dom a bset) (get_domtab r1)
  |> Cod.NTab.fold (fn (b,aset) => add_to_cod b aset) (get_codtab r1)

(* convert to/from lists *)
val list_of = Dom.NTab.list_of o get_domtab;
fun of_list l = fold (fn (a,bset) => add_to_dom a bset) l empty;

val rawlist_of = maps (fn (a,bset) => map (pair a) (Cod.NSet.list_of bset))
                 o Dom.NTab.list_of o get_domtab;
fun of_rawlist l = fold (fn (a,b) => add1 a b) l empty;

(* deleting all entries *)
fun del_dom a1 rel = 
    (case Dom.NTab.lookup (get_domtab rel) a1 of 
       NONE => rel
     | SOME changedbs => rel |> del_from_dom a1 changedbs);

fun del_cod b1 rel = 
    (case Cod.NTab.lookup (get_codtab rel) b1 of 
       NONE => rel
     | SOME changedas => rel |> del_from_cod b1 changedas);

(* renaming *)

(* rename domain element *)    
fun rename1_opt_dom a1 a2 rel = 
    let val domtab = get_domtab rel
    in 
      case Dom.NTab.lookup domtab a1 of 
        NONE => (NONE, rel)
      | SOME codset => 
        let val (a3opt,domtab2) = Dom.NTab.rename1_opt a1 a2 domtab
            val a3 = case a3opt of NONE => a2 | SOME a3' => a3'
        in 
          (a3opt,
           rel |> set_domtab domtab2
               |> update_codf 
               (fn codtab => 
                   Cod.NSet.fold 
                     (Cod.NTab.map_entry (snd o Dom.NSet.rename1 a1 a3))
                     codset codtab))
        end
    end;

fun rename1_dom a1 a2 rel = 
    let val domtab = get_domtab rel
    in 
      case Dom.NTab.lookup domtab a1 of 
        NONE => (a2, rel)
      | SOME codset => 
        let val (a3,domtab2) = Dom.NTab.rename1 a1 a2 domtab
        in 
          (a3,
           rel |> set_domtab domtab2
               |> update_codf 
               (fn codtab => 
                   Cod.NSet.fold 
                     (Cod.NTab.map_entry (snd o Dom.NSet.rename1 a1 a3))
                     codset codtab))
        end
    end;

fun try_rename1_dom a1 a2 rel = 
    let val domtab = get_domtab rel
    in case Dom.NTab.try_rename1 a1 a2 domtab of 
         NONE => NONE
       | SOME domtab2 => 
         (case Dom.NTab.lookup domtab a1 of 
            NONE => SOME rel
          | SOME codset => 
            SOME (rel |> set_domtab domtab2
                      |> update_codf 
                      (fn codtab => 
                          Cod.NSet.fold 
                            (Cod.NTab.map_entry (snd o Dom.NSet.rename1 a1 a2))
                            codset codtab)))
    end;

(* rename codomain element *)    
fun rename1_opt_cod a1 a2 rel = 
    let val codtab = get_codtab rel
    in 
      case Cod.NTab.lookup codtab a1 of 
        NONE => (NONE, rel)
      | SOME domset => 
        let val (a3opt,codtab2) = Cod.NTab.rename1_opt a1 a2 codtab
            val a3 = case a3opt of NONE => a2 | SOME a3' => a3'
        in 
          (a3opt,
           rel |> set_codtab codtab2
               |> update_domf 
               (fn domtab => 
                   Dom.NSet.fold 
                     (Dom.NTab.map_entry (snd o Cod.NSet.rename1 a1 a3))
                     domset domtab))
        end
    end;

fun rename1_cod a1 a2 rel = 
    let val codtab = get_codtab rel
    in 
      case Cod.NTab.lookup codtab a1 of 
        NONE => (a2, rel)
      | SOME domset => 
        let val (a3,codtab2) = Cod.NTab.rename1 a1 a2 codtab in 
          (a3,
           rel |> set_codtab codtab2
               |> update_domf 
               (fn domtab => 
                   Dom.NSet.fold 
                     (Dom.NTab.map_entry (snd o Cod.NSet.rename1 a1 a3))
                     domset domtab))
        end
    end;

fun try_rename1_cod a1 a2 rel = 
    let val codtab = get_codtab rel
    in case Cod.NTab.try_rename1 a1 a2 codtab of 
         NONE => NONE
       | SOME codtab2 => 
         (case Cod.NTab.lookup codtab a1 of 
            NONE => SOME rel
          | SOME domset => 
           SOME (rel |> set_codtab codtab2
                     |> update_domf 
                     (fn domtab => 
                         Dom.NSet.fold 
                           (Dom.NTab.map_entry (snd o Cod.NSet.rename1 a1 a2))
                           domset domtab)))
    end;


(* pretty printing *)
fun pretty rel = 
    Pretty.chunks 
    [Pretty.str "Rel{",
     Pretty.block [Pretty.str "Dom->Cod:",  
                   Dom.NTab.pretty Cod.NSet.pretty (get_domtab rel)],
     Pretty.block [Pretty.str "Cod->Dom:",  
                   Cod.NTab.pretty Dom.NSet.pretty (get_codtab rel)],
     Pretty.str "}"];
val print = Pretty.writeln o pretty;


structure Sharing = struct
  structure Dom = Dom.Sharing;
  structure Cod = Cod.Sharing;
  type T = T;
end; (* sharing struct *)

end;


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*)
(* two (more) instances of type-distinct binary relations 
   (sharing Dom and Cod) *)
signature TWO_CONV_NAME_BINRELS
= sig
structure R : NAME_BINREL
structure A : NAME_BINREL
structure B : NAME_BINREL
val coerceRA :  R.T -> A.T
val coerceRB :  R.T -> B.T
val coerceAR :  A.T -> R.T
val coerceBR :  B.T -> R.T
sharing A.Dom.Sharing = R.Dom.Sharing
sharing A.Cod.Sharing = R.Cod.Sharing
sharing B.Dom.Sharing = R.Dom.Sharing
sharing B.Cod.Sharing = R.Cod.Sharing
end;


functor TwoRelsFun(R : NAME_BINREL) : TWO_CONV_NAME_BINRELS
= struct
  structure R = R;
  structure A = R;
  structure B = R;
  fun coerceRB (x : R.T) = x;
  fun coerceRA (x : R.T) = x;
  fun coerceAR x  = (x : R.T);
  fun coerceBR x = (x : R.T);
end;
