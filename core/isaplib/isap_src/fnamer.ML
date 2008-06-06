(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      gproof/tools/fnamer.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                Initially written Oct 2005, Last Updated 29 Jan 2008   *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

    Used to get new nice fresh names (quickly). With a slight
    adaptation, it can also be thought of as a memory management where
    names are memory spaces.

    A descrete interval table (for fresh names): a specialised, AVL
    tree inspired, representation for holding intervals and getting
    quick lookup/add/delete, worst case O(Log(n)^2); but when adjacent
    names are used it is much faster (often constant time).

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)


(* A namer *)
  (* properties: 
     suc and pred inverse
     ord (pred x, x) == less (optional for bottom element)
     ord (suc x, x) == more

     pred can have a lower bound, such that (pred x = x) but suc
     cannot have an upper bound.
  *)
signature NAMER
= sig
  type name 
  val suc : name -> name
  val pred : name -> name option
  val ord : name * name -> order
  val pretty : name -> Pretty.T
end;

(* a namer that has names made from a src type assumes: (mk (dest n) = n) *)
signature INTERP_NAMER
= sig
type src 
include NAMER
val mk : src -> name
val dest : name -> src
end;

(* a functor to make a an opaque namer from a src namer type, this is
   really just for name hidding. *)
functor InterpNamerFun(N : NAMER) :> INTERP_NAMER
= struct
type src = N.name;
datatype name = Name of src;
fun dest (Name n) = n;
val mk = Name;
fun suc (Name n) = Name (N.suc n);
fun pred (Name n) = (case N.pred n of NONE => NONE | SOME n2 => SOME (Name n2));
fun ord (Name n1, Name n2) = N.ord (n1,n2);
fun pretty (Name n) = N.pretty n;
end;

signature IDEAL_NAMER
= sig
structure Src : NAMER
include NAMER
val mk : Src.name (* ideal *)
				 * Src.name (* actual *)
				 -> name
val get_ideal : name -> Src.name (* each name has an ideal name it wants to be *)
val get_actual : name -> Src.name (* each name has an ideal name it wants to be *)
end;

(* a functor to make an ideal namer from a src namer type, this is
   really just for name hidding. It allows us to try to make every
   name closer to the idea, for example, when used name is removed, we
   might be able to rename later names. *)
functor IdealNamerFun(N : NAMER) :> IDEAL_NAMER
= struct
structure Src = N;
datatype name = Name of N.name (* ideal *)
												* N.name; (* actual *)
val mk = Name;
fun suc (Name (i,n)) = Name (i,N.suc n);
fun pred (Name (i,n)) = (case N.pred n of NONE => NONE 
																				| SOME n2 => SOME (Name (i,n2)));
fun ord (Name ns1, Name ns2) = Library.prod_ord N.ord N.ord (ns1, ns2);
fun pretty (Name (_,n)) = N.pretty n;
fun get_ideal (Name (i,_)) = i;
fun get_actual (Name (_,n)) = n;
end;


(* A Fresh Namer : this holds collection of used names in order to
   make fresh ones *)
signature FNAMER =
sig
  type name
  structure N : NAMER sharing type N.name = name;

datatype T 
  = Empty
  | Br of int (* depth of stuff this branch *)
          * (N.name * N.name) (* inclusive min/max range *)
          * T * T; (* subtrees with smaller than min in left and bigger than max in right *)
(* 
  type T
*)

  exception empty_exp of string;
  exception duplicate_exp of N.name * T;

  val empty : T
  val is_empty : T -> bool
  val is_singleton : T -> bool
  val tryget_singleton : T -> N.name option
  val get_min : T -> N.name option (* lowest value, log time *)
  val get_max : T -> N.name option (* highest value, log time *)
  val get_local_bot : T -> N.name option (* random middle name - constant time *)
  val get_local_top : T -> N.name option (* random middle name - constant time *)
  val get_first : T -> N.name option (* smallest name - log time *)
  val get_last : T -> N.name option (* largest name - log time *)
  
  (* fold over ranges *)
  val fold_ranges : ((N.name * N.name) -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_ranges_rev : ((N.name * N.name) -> 'a -> 'a) -> T -> 'a -> 'a
  (* fold all elements *)
  val fold : (N.name -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_rev : (N.name -> 'a -> 'a) -> T -> 'a -> 'a

  val dest : T -> N.name list
  val of_list : N.name list -> T

  val next_smaller : T -> N.name -> N.name option
  val next_bigger : T -> N.name -> N.name option

  val add_new : N.name -> T -> (N.name * T)
  val ins_fresh : N.name -> T -> T (* raises duplicate_exp *)
  val new : N.name -> T -> N.name
  val add : N.name -> T -> T (* = union1 *)
  val add_list : N.name list -> T -> T
  val add_range : N.name * N.name -> T -> T
  val add_ranges : T -> (N.name * N.name) list -> (N.name * N.name) list

  val lookup : T -> N.name -> (N.name * N.name) option
  val contains : T -> N.name -> bool
  val delete : N.name -> T -> T
  val delete1 : N.name -> T -> T (* for debgging only *)

  val union_merge : T -> T -> T (* union *)
  val subtract : T -> T -> T (* subtraction = (fst - snd) *)
  val remove_set : T -> T -> T (* subtraction = (snd - fst) *)
  val intersect : T -> T -> T (* intersection *)

  (* pull out the biggest and smallest range elements (log time),
     raises empty_exp is empty *)
  val pop_biggest : T -> (N.name * N.name) * T;
  val pop_smallest : T -> (N.name * N.name) * T;

  (* pull from the middle range, as value (either the top or bot end)
     out from the name table (constant time), give back element and
     new table; NONE if empty; *)
  val pull_local_top : T -> (N.name * T) option;
  val pull_local_bot : T -> (N.name * T) option;

  (* make the list of every element pulled out and paired with the rest *)
  val pull_each : T -> (N.name * T) list

  (* check subset relation *)
  val sub_set : T -> T -> bool

  (* the list of all subsets *)
  val powerset : T -> T list

  (* assumes a in tr; returns { ((leq a tr) UN x) | a <= x & x in tr }, 
     where (leq a tr) is elements less than or equal to a in tr. *) 
  val powerset_geq : N.name -> T -> T list

  (* check if tree is balanced everywhere *)
  val is_fully_balanced : T -> bool

  (* internal: rebalances w.r.t. only top level inbalance. 
     log time worst case. *)
  val fully_rebalance : T -> T

  (* Given a tree which is unbalanced at the top level, but with balanced
     subtrees, rebalance it. *)
  val rebalance : T -> T

  (* internal *)
  val join_from_delete : T -> T -> T
  val find_and_delete_bot_join : N.name * N.name -> T -> N.name * T
  val find_and_delete_top_join : N.name * N.name -> T -> N.name * T

  val pretty_full : T -> Pretty.T
  val print_full : T -> unit
  val pretty_struct : T -> Pretty.T
  val print_struct : T -> unit
  val pretty : T -> Pretty.T
  val print : T -> unit

  val assert_correct : T -> T (* raises if not correct *)

  (* should never happen *)
  exception delete_exp of string * N.name * T;
  exception add_range_exp of string * (N.name * N.name) * T;
  exception add_exp of string * N.name * T;
  exception union_merge_exp of string * T * T;

  exception bug0_exp of string;
  exception bug_exp of string * T;
  exception bug2_exp of string * T * T;
  exception bad_fnametab_exp of string * T;

end;


(* Fresh Name Table Functor *)
functor FNamerFun(N : NAMER)
(* : FNAMER  *)
= struct

structure N = N;

type name = N.name;

(* used to allow the bottom element to be its own pred; thus we can
have a fixed bottom point *)
fun pred_or_same s = case N.pred s of NONE => s | SOME ps => ps;

(* invarient: as for balanced search trees (AVL trees): left is less,
right is bigger, max difference in height is 1 *)
datatype T 
  = Empty
  | Br of int (* depth of stuff this branch *)
          * (N.name * N.name) (* inclusive min/max range *)
          * T * T; (* subtrees with smaller than min in left and bigger than max in right *)

(* hopefully impossible exceptions for debugging *)
exception delete_exp of string * N.name * T;
exception add_range_exp of string * (N.name * N.name) * T;
exception add_exp of string * N.name * T;
exception union_merge_exp of string * T * T;

          
(* hopefully impossible exceptions *)
exception bug0_exp of string;
exception bug_exp of string * T;
exception bug2_exp of string * T * T;
exception duplicate_exp of N.name * T;

(* table was empty *)
exception empty_exp of string;
(* table was badly formed... *)
exception bad_fnametab_exp of string * T;

(* helper things *)
fun is_less a b = case N.ord (a,b) of LESS => true | _ => false;
fun is_leq a b = case N.ord (a,b) of GREATER => false | _ => true;
fun is_eq a b = case N.ord (a,b) of EQUAL => true | _ => false;

(* compute the depth of a tree *)
fun real_depth_of Empty = 0
  | real_depth_of (Br (_,_,l,r)) = 
    1 + (Int.max (real_depth_of l, real_depth_of r));
(* lookup the stored depth *)
fun depth_of Empty = 0
  | depth_of (Br (i,_,_,_)) = i;

(* bottom and top of a branch range *)
fun get_min (Br (_, (b,t),Empty,r)) = SOME b
  | get_min (Br (_, (b,t),l,r)) = get_min l
  | get_min Empty = NONE;
fun get_max (Br (_, (b,t),l,Empty)) = SOME t
  | get_max (Br (_, (b,t),l,r)) = get_max r
  | get_max Empty = NONE;

(* (local) check if a tree is balanced - according to it's stored depths at
the top level. *)
fun is_locally_balanced Empty = true
  | is_locally_balanced (Br(_,v, l, r)) =
    (Int.abs ((depth_of l) - (depth_of l))) < 2;

(* (global) check the the tree is correctly balanced at every node in
the tree. *)
fun is_fully_balanced Empty = true
  | is_fully_balanced (tr as Br(_,v, l, r)) =
    is_locally_balanced tr 
    andalso is_fully_balanced l 
    andalso is_fully_balanced r;

(* correctness checking functions (these are supposed to be invarient 
   over top-level functions): *)
fun assert_balanced tr = 
    is_fully_balanced tr orelse 
    raise bad_fnametab_exp ("assert_balanced",tr);   

fun assert_correct_depths Empty = true
  | assert_correct_depths (tr as Br (d,(b,t),l,r)) = 
    if depth_of tr <> real_depth_of tr then 
      raise bad_fnametab_exp ("assert_correct_depths",tr)
    else
      assert_correct_depths l andalso assert_correct_depths r;

(* IMPROVE: very inefficient: lots of re-computing of max r and max l *)
fun assert_correctly_ordered Empty = true
  | assert_correctly_ordered (Br (d,(b,t),l,r)) = 
    if (case (get_min r) of NONE => false | SOME rv => is_leq rv t) orelse
       (case (get_max l) of NONE => false | SOME lv => is_leq b lv) orelse 
       is_less t b
    then raise bad_fnametab_exp ("assert_correctly_ordered",Br (d,(b,t),l,r))
    else assert_correctly_ordered l andalso assert_correctly_ordered r;

fun assert_correct t = 
    (assert_balanced t; 
     assert_correct_depths t; 
     assert_correctly_ordered t; t);


(* basic constructor *)
val empty = Empty;

(* depth of a br tree with these subtrees *)
fun br_depth Empty Empty = 1
  | br_depth (Br(d,_,_,_)) Empty = d + 1
  | br_depth Empty (Br(d,_,_,_)) = d + 1
  | br_depth (Br(dl,_,_,_))  (Br(dr,_,_,_)) = 
    1 + Int.max (dl,dr);

(* depth of a br tree given one child and where the other child has depth d *)
fun br_depth1 Empty d = d
  | br_depth1 (Br(d,_,_,_)) d2 = 1 + Int.max (d,d2);


(* IMPROVE: use efficient fold over to replace fold and dest... *)
(* add tree ranges to a list of ranges *)
fun add_ranges Empty L = L
  | add_ranges (Br (_, g, l, r)) L =
    add_ranges l (g :: add_ranges r L);

fun dest_range (b,t) = 
    (case N.ord (b, t) 
      of EQUAL => [b]
       | GREATER => [] (* should not happen - new exception? *)
       | LESS => b :: (dest_range (N.suc b, t)));

(* give back list of all used names, expands the intervals *)
fun dest tr = maps dest_range (add_ranges tr []);

(* empty/singleton checks *)
fun is_empty Empty = true
  | is_empty _ = false;
fun is_singleton (Br (_, g, Empty, Empty)) = 
    (case N.ord g of EQUAL => true | _ => false)
  | is_singleton _ = false;
fun tryget_singleton (Br (_, g as (b,t), Empty, Empty)) = 
    (case N.ord g of EQUAL => SOME b | _ => NONE)
  | tryget_singleton _ = NONE;


(* element is below tree ranges *)
fun less_than s (Br (_, (b,t),l,r)) = (N.ord(s,b) <> GREATER)
  | less_than s Empty = true;
(* element is above tree ranges *)
fun more_than s (Br (_, (b,t),l,r)) = (N.ord(s,t) <> LESS)
  | more_than s Empty = true;
(* element is within tree ranges *)
fun within s (b,t) = (N.ord(s,b) <> LESS) andalso (N.ord(s,t) <> GREATER);

(* bottom and top of a branch range *)
fun get_local_bot (Br (_, (b,t),l,r)) = SOME b
  | get_local_bot _ = NONE;
fun get_local_top (Br (_, (b,t),l,r)) = SOME t
  | get_local_top _ = NONE;


(* first and last name *)
fun get_first (Br (_, (b,t),Empty,r)) = SOME b
  | get_first (Br (_, (b,t),l,r)) = get_first l
  | get_first Empty = NONE;
fun get_last (Br (_, (b,t),l,Empty)) = SOME t
  | get_last (Br (_, (b,t),l,r)) = get_last l
  | get_last Empty = NONE;


(* Given a tree which is unbalanced at the top level, but whos
subtrees are balanced, rebalance it. -- I think this is optimal -
think of actual runtime -- log (empty, N), requires log N shuffles. *)
fun rebalance (Br(_,v, Empty, Empty)) = (Br(1,v,Empty,Empty))
  | rebalance (Br(_,v, Empty, r as Br(d, v2, l2, r2))) = 
    if d > 1 then 
      let val newl = rebalance(Br(depth_of l2,v,Empty,l2))
      in Br(br_depth newl r2,v2,newl,r2) end
    else Br(2,v,Empty,r) (* no rebalancing needed *)
  | rebalance (Br(_,v, l as Br(d,v2,l2,r2), Empty)) = 
    if d > 1 then 
      let val newr = rebalance(Br(depth_of r2,v,r2,Empty))
      in Br(br_depth l2 newr,v2,l2,newr) end
    else Br(2,v,l,Empty) (* no rebalancing needed *)
  | rebalance (tr as Br(_,v, l as Br(ld,lv,ll,lr), r as Br(rd,rv,rl,rr))) = 
    let val dd = ld - rd in
      if dd > 1 then 
        let val newr = rebalance(Br(br_depth lr r,v,lr,r))
        in Br(br_depth newr ll,lv,ll,newr) end
      else if dd < ~1 then 
        let val newl = rebalance(Br(br_depth l rl,v,l,rl))
        in Br (br_depth newl rr,rv,newl,rr) end
      else tr
    end
  | rebalance Empty = Empty;


(* completely re-balance a tree, in case it's generated randomly:
equiv to sorting, n log n time at worst. Assumes correct cached depth
values *)
fun fully_rebalance Empty = Empty
  | fully_rebalance (Br(d,v,l,r)) = 
    rebalance (Br(d,v,fully_rebalance l,fully_rebalance r));



(* pulls out the biggest element, also giving back new balanced tree,
   assumes initial true was well formed (especially balanced). *)
fun pop_biggest Empty = raise empty_exp "pop_biggest"
  | pop_biggest (Br(d,v,l,Empty)) = (v, l)
  | pop_biggest (Br(d,v,l,r)) = 
    let val (biggest,newr) = pop_biggest r
    in (biggest, rebalance (Br(br_depth l newr,v,l,newr))) end;

fun pop_smallest Empty = raise empty_exp "pop_smallest"
  | pop_smallest (Br(d,v,Empty,r)) = (v, r)
  | pop_smallest (Br(d,v,l,r)) = 
    let val (smallest,newl) = pop_smallest l
    in (smallest, rebalance (Br(br_depth newl r,v,newl,r))) end;

(* for use when these are the children of a deleted node, in order to
   make the replacement for the deleted node. *)
fun join_from_delete Empty Empty = Empty
  | join_from_delete (tr as Br _) Empty = tr
  | join_from_delete Empty (tr as Br _) = tr
  | join_from_delete (l as Br(ld,lv,ll,lr)) (r as Br(rd,rv,rl,rr)) = 
    if ld >= rd then 
      let val (newv, newl) = pop_biggest l 
      in Br(br_depth newl r, newv, newl, r) end
    else 
      let val (newv, newr) = pop_smallest r 
      in Br(br_depth l newr, newv, l, newr) end;

(* Note: Depth is recalculated in rebalance. *)
(* find_and_delete_bot_join (sx,sop) tr *)
(* where 
     1) sp = Pred sx
     2) (tr, vrange, trr) is a valid tree (the parent tree)
     3) Suc sx = bottom(vrange)
     Inferable: sx > tr
   returns: (new bottom value, new left-tree)
*)
fun find_and_delete_bot_join (s as (sx,sp)) Empty = (sx,Empty)
  | find_and_delete_bot_join (s as (sx,sp)) (Br(d,v as (b,t),l,r)) = 
    (case N.ord(sp,t) of
       EQUAL => (b, l) (* r must be empty by (1) and (3) *)
     | GREATER => let val (s',r') = find_and_delete_bot_join s r
                  in (s',rebalance (Br(br_depth l r',v,l,r'))) end
     | LESS => (* implies sx <= t, and thus by (3) that this tree 
                  should already be joined to the parent *)
       raise bug_exp ("find_and_delete_bot_join: badly formed tree",
                      (Br(d,v,l,r))));
(* 
let val (s',l') = find_and_delete_bot_join s l
                  in (s',rebalance (Br(d,v,l',r))) end;
*)

fun find_and_delete_top_join (s as (sx,ss)) Empty = (sx,Empty)
  | find_and_delete_top_join (s as (sx,ss)) (Br(d,v as (b,t),l,r)) = 
    (case N.ord(ss,b) of
       EQUAL => (t, r) (* by (1) and (3), l must be empty *)
     | GREATER => (* implies sx >= b, and thus by (3) that this tree 
                  should already be joined to the parent *)
       raise bug_exp ("find_and_delete_top_join: badly formed tree",
                      (Br(d,v,l,r)))
     | LESS => let val (s',l') = find_and_delete_top_join s l
                  in (s',rebalance (Br(br_depth l' r,v,l',r))) end);

(* let val (s',r') = find_and_delete_top_join s r
                  in (s',rebalance (Br(d,v,l,r'))) end
*)

(* add a new value s into the name table *)
fun add' (s as (sp,sx,ss)) Empty = Br (1,(sx,sx),Empty,Empty)
  | add' (s as (sp,sx,ss)) (tr as Br (d,v as (b,t),l,r)) = 
    if within sx v then tr
    else (
      case N.ord(sx,b) of 
        LESS =>
        (case N.ord(ss,b) (* check for bot join *)
           of EQUAL => 
              let val (newb,newl) = find_and_delete_bot_join (sx,sp) l
              in rebalance (Br (br_depth newl r,(newb,t),newl,r)) end
            | _ => let val newl = add' s l
                   in rebalance (Br (br_depth newl r,(b,t),newl,r)) end)
      | _ => 
        (case N.ord(sp,t) (* check for top join *)
          of EQUAL => 
             let val (newt,newr) = find_and_delete_top_join (sx,ss) r
             in rebalance (Br (br_depth l newr,(b,newt),l,newr)) end
                  (* if completely separate, and not less, put on right *)
           | _ => let val newr = add' s r 
                  in rebalance (Br (br_depth l newr,(b,t),l,newr)) end)  
      );

(* 
fun add s = add' (pred_or_same s, s, N.suc s);
fun add s = assert_correct o add' (pred_or_same s, s, N.suc s); 
*)

fun add2 s tr = 
    assert_correct (add' (pred_or_same s, s, N.suc s) tr) 
    handle bad_fnametab_exp (str,_) => raise add_exp (str, s, tr);

val add = add2;


fun add_list l = fold add l;
fun of_list l = assert_correct (add_list l empty);

(* lookup the range that a value is in. returns NONE if not in tree *)
fun lookup tr s = 
    let 
      fun lookup' Empty = NONE
        | lookup' (tr as Br (d,v as (b,t),l,r)) = 
          (case N.ord(s,b) 
            of GREATER => 
               (case N.ord(s,t)
                of GREATER => lookup' r
                  | _ => SOME v)
             | EQUAL => SOME v
             | LESS => lookup' l)
    in lookup' tr end

fun contains tr s = case lookup tr s of NONE => false | SOME _ => true;

(* find the next entry in the name collection smaller than s *)
fun next_smaller tr s = 
    let 
      fun next' Empty = NONE
        | next' (tr as Br (d,v as (b,t),l,r)) = 
          (case N.ord(s,b)
            of GREATER => (case N.ord(s,t) 
                            of GREATER => (case next' r of NONE => SOME t | x => x)
                             | _ => SOME (pred_or_same s))
             | EQUAL => get_last l
             | LESS => next' l)
    in next' tr end;

(* find the next entry in the name collection bigger than s *)
fun next_bigger tr s = 
    let 
      fun next' Empty = NONE
        | next' (tr as Br (d,v as (b,t),l,r)) = 
          (case N.ord(s,t) 
            of GREATER => next' r
             | EQUAL => get_first r
             | LESS => (case N.ord(s,b)
                         of LESS => (case next' l of NONE => SOME b | x => x)
                          | _ => SOME (N.suc s)))
    in next' tr end;

(* adding new elements *)
fun add_new a tr = 
    case lookup tr a of 
      NONE => (a, add a tr)
    | SOME (b,t) => 
      let val a2 = N.suc t in (a2, add a2 tr) end;

fun new a tr = 
    case lookup tr a of 
      NONE => a
    | SOME (b,t) => 
      let val a2 = N.suc t in a2 end;

fun ins_fresh a tr = 
    case lookup tr a of 
      NONE => add a tr
    | _ => raise duplicate_exp (a,tr);


(* grow from s getting smaller. Gives back new max and rest of tree.
   will break balancing of tree. Given a new lowest value, extend the
   tree, giving back the new lowest value and the new subtree (balanced) *)
fun grow_left s Empty = (s,Empty)
  | grow_left s (Br(d,v as (b,t),l,r)) = 
    (case N.ord (s,b) 
      of LESS => grow_left s l
       | _ => (case N.ord (s, N.suc t) 
               of GREATER => 
                  let val (s', r') = grow_left s r
                  in (s', rebalance (Br(br_depth r' l,v,l,r'))) end
                | _ => (b, l)));

(* grow from s getting bigger. Gives back new max and rest of tree.
   Will break balancing of tree *)
fun grow_right s Empty = (s,Empty)
  | grow_right s (Br(d,v as (b,t),l,r)) = 
    (case N.ord (s,t) 
      of GREATER => grow_right s r
       | _ => (case N.ord (N.suc s,b) 
               of LESS => 
                  let val (s', l') = grow_right s l
                  in (s', rebalance (Br(br_depth l' r,v,l',r))) end
                | _ => (t, r)));

fun min_name n n2 = case N.ord (n,n2) of LESS => n | _ => n2;
fun max_name n n2 = case N.ord (n,n2) of LESS => n2 | _ => n;

(* without rebalancing *)
fun add_range (v as (b,t)) Empty = Br (1,v,Empty,Empty)
  | add_range (v as (b,t)) (tr as Br (d,v2 as (b2,t2),l,r)) = 
    rebalance 
      (case N.ord (b,b2)
        of LESS => (* left is below this node's left *)
           (case N.ord (N.suc t,b2) 
             of LESS => (* strictly less than this node *)
                let val l' = add_range v l 
                in Br (1 + Int.max(depth_of l', depth_of r),v2,l',r) 
                end
              | _ => (* top of added range touches left of this node *)
                let val (b',l') = grow_left b l in 
                  (case N.ord (t,t2) 
                    of GREATER => (* completely overlaps this node *)
                       let val (t',r') = grow_right t r
                       in (Br (1 + Int.max(depth_of l', depth_of r'),
                               (b',t'),l',r')) end
                     | _ => Br (d,(b',t2),l',r))
                end)
         | _ => 
           (case N.ord (pred_or_same b,t2) 
             of GREATER => (* strictly bigger than this node *)
                let val r' = add_range v r 
                in Br (1 + Int.max(depth_of l, depth_of r'),v2,l,r') end
              | _ => (* bottom of added range overlaps on this node *) 
                (case N.ord (t,t2) 
                  of GREATER => (* top overhangs to the right *)
                     let val (t',r') = grow_right t r
                     in Br (1 + Int.max(depth_of l, depth_of r'),
                            (min_name b b2,t'),l,r') end
                   | _ => (* within this node *)
                     tr)));


(* ? THINK: is this the most efficient way ? *)
fun union_merge Empty tr = tr
  | union_merge (Br(d,v,l,r)) tr = 
    union_merge r (union_merge l (add_range v tr));
val union_merge = assert_correct oo union_merge;


(* fast(est?) sub_set check arg1 is a subset of arg2 *)
fun sub_set Empty tr = true
  | sub_set (Br _) Empty = false
  | sub_set (n1 as Br(d1,(b1,t1),l1,r1)) (n2 as Br(d2,(b2,t2),l2,r2)) = 
    (case N.ord (t1,b2) 
      of LESS => sub_set n1 l2
       | EQUAL => (case N.ord (b1, b2) 
                    of EQUAL => sub_set l1 l2 andalso sub_set r1 n2
                     | _ => false)
       | GREATER => 
         (case N.ord (b1,t2)
           of LESS => (case N.ord (b1,b2) 
                        of EQUAL => (case N.ord (t1,t2) 
                                      of EQUAL => true 
                                       | _ => false)
                         | _ => false)
            | EQUAL => 
              (case N.ord (t1, t2) 
                of EQUAL => sub_set l1 n2 andalso sub_set r1 l2
                 | _ => false)
            | GREATER => sub_set n1 r2));    

(* delete - consider cases: 1. s is in the middle of a range, then we
need to split the range and insert the appropriate half into the
shorter branch of the tree. 2. s is the end of some range, then we
just move the range along one bit. But when the range we are adjusting
is just one element, then we simply remove this node, as per avl
trees. 3. recurse on left or right *)
fun delete s Empty = Empty
  | delete s (tr as Br (d,v as (b,t),l,r)) = 
    (case (N.ord(s,b), N.ord(s,t))
      of (EQUAL,EQUAL) => 
         join_from_delete l r
       | (GREATER,LESS) => 
         (case Int.compare (depth_of l, depth_of r)
           of LESS => Br (d,(N.suc s,t),add_range (b,pred_or_same s) l,r)
            | EQUAL => 
              let val newl = add_range (b,pred_or_same s) l
              in Br (br_depth newl r,(N.suc s,t),newl,r) end
            | MORE => Br (d,(b,pred_or_same s),l,add_range (N.suc s,t) r))
       | (EQUAL, LESS) => Br (d,(N.suc b,t),l,r)
       | (GREATER, EQUAL) => Br (d,(b,pred_or_same t),l,r)
       | (LESS, LESS) => 
         let val newl = delete s l
         in rebalance (Br (br_depth newl r,v,newl,r)) end
       | (GREATER, GREATER) => 
         let val newr = delete s r
         in rebalance (Br (br_depth l newr,v,l,newr)) end
       | (EQUAL, GREATER) => raise bug0_exp "impossible! delete 1"
       | (LESS, EQUAL) => raise bug0_exp "impossible! delete 2"
       | (LESS, GREATER) => raise bug0_exp "impossible! delete 3");

val delete1 = delete;

fun delete' s t = 
    assert_correct (delete s t)
    handle bad_fnametab_exp (msg,_) => raise delete_exp ("delete bug: " ^ msg, s, t)
         | bug0_exp msg => raise delete_exp ("delete bug: " ^ msg, s, t);

val delete = delete';

(* pulls out the biggest element, also giving back new balanced tree,
   assumes initial true was well formed (especially balanced). *)
fun pull_local_top Empty = NONE
  | pull_local_top (tr as (Br(_,(b,t),_,_))) = SOME (t, delete t tr);

fun pull_local_bot Empty = NONE
  | pull_local_bot (tr as Br(_,(b,t),_,_)) = SOME (b, delete b tr);


    

(* 
fun delete_range delme Empty = Empty
  | delete_range (delb,delt) (tr as Br (d,v as (b,t),l,r)) = 
    (case (N.ord(s,b), N.ord(s,t))
      of (EQUAL,EQUAL) => 
         join_from_delete l r
       | (GREATER,LESS) => 
         (case Int.compare (depth_of l, depth_of r)
           of LESS => Br (d,(N.suc s,t),add_range (b,pred_or_same s) l,r)
            | EQUAL => Br (d + 1,(N.suc s,t),add_range (b,pred_or_same s) l,r)
            | MORE => Br (d,(b,pred_or_same s),l,add_range (N.suc s,t) r))
       | (EQUAL, LESS) => Br (d,(N.suc b,t),l,r)
       | (GREATER, EQUAL) => Br (d,(b,pred_or_same t),l,r)
       | (LESS, LESS) => rebalance (Br (d,v, delete s l, r))
       | (GREATER, GREATER) => rebalance (Br (d,v, l, delete s r))
       | (EQUAL, GREATER) => raise delete_exp "impossible! delete 1"
       | (LESS, EQUAL) => raise delete_exp "impossible! delete 2"
       | (LESS, GREATER) => raise delete_exp "impossible! delete 3")
*)

(* efficient computation of powerset *)

(* assumes a in tr; returns { ((leq a tr) UN x) | a <= x & x in tr }, 
   where (leq a tr) is elements less than or equal to a in tr. *) 
fun powerset_geq a tr = 
    (case next_bigger tr a
      of NONE => [add a empty,empty]
       | SOME b =>
         let val bsets = powerset_geq b (delete a tr)
         in (map (add a) bsets) @ bsets end);

(* returns powerset of tr *)
fun powerset tr = 
    (case get_first tr of 
       NONE => []
     | SOME a => powerset_geq a tr)


(* fold over the ranges in the name tree *)
fun fold_ranges f Empty a = a
  | fold_ranges f (tr as Br (d,v as (b,t),l,r)) a = 
    a |> fold_ranges f l
      |> f (b,t)
      |> fold_ranges f r;

fun fold_ranges_rev f Empty a = a
  | fold_ranges_rev f (tr as Br (d,v as (b,t),l,r)) a = 
    a |> fold_ranges_rev f r
      |> f (b,t)
      |> fold_ranges_rev f l;

(* IMPROVE: THIS IS NOT EFFICIENT! *)
(* subtract 1st from 2nd = fromthis - subtractthis *)
fun subtract fromthis subtractthis =  
    fold delete (dest subtractthis) fromthis;
val subtract = assert_correct oo subtract;
(* same as above but swaped arguments for curried folding *)
fun remove_set subtractthis fromthis =  
    fold delete (dest subtractthis) fromthis;
val remove_set = assert_correct oo subtract;

(* IMPROVE: this is the inefficient version; write the efficien one ! *)
fun intersect A B = 
    fold (fn a => if contains B a then I else delete a) (dest A) A;


fun pretty_range (b,t) = 
    (case N.ord (b,t) of 
       EQUAL => N.pretty b
     | _ => 
       Pretty.block [Pretty.str "[", N.pretty b, Pretty.str " - ",
                     N.pretty t, Pretty.str "]" ]);

(* pretty printing in full detail: shows internal tree structure *)
fun pretty_full Empty = Pretty.block []
  | pretty_full (Br (d,bt,l,r)) = 
    Pretty.chunks 
      ([Pretty.block
          [ Pretty.str (Int.toString d), Pretty.str " : ", pretty_range bt]]
       @ [Pretty.indent 2 (case l of Empty => Pretty.str "Empty" 
                                   | _ => pretty_full l)]
       @ [Pretty.indent 2 (case r of Empty => Pretty.str "Empty"
                                   | _ => pretty_full r)]);

(* pretty printing in brief set style, list of results *)
fun prettyL Empty = []
  | prettyL (Br (d,bt,l,r)) = 
    ((prettyL l) @ [pretty_range bt] @ (prettyL r));

fun pretty_struct' Empty = []
  | pretty_struct' (Br (d,bt,l,r)) = 
    [Pretty.enclose "(" ")," (pretty_struct' l)]
    @ [pretty_range bt]
    @ [Pretty.enclose ", (" ")" (pretty_struct' r)];

fun pretty ns =  Pretty.list "{" "}" (prettyL ns);
fun pretty_struct ns =  Pretty.enclose "{" "}" (pretty_struct' ns);


val print = Pretty.writeln o pretty;
val print_struct = Pretty.writeln o pretty_struct;
val print_full = Pretty.writeln o pretty_full;


(* fold over all elements *)
fun fold f = fold_ranges (Basics.fold f o dest_range);
fun fold_rev f = fold_ranges_rev (Basics.fold_rev f o dest_range);

(* {a,b,c,d} => [(a, {b,c,d}), (b, {a,c,d}), (c,{a,b,d}), ... ] *)
fun pull_each nset = fold (fn n => fn l => (n,delete n nset)::l) nset [];


end; (* structure *)


