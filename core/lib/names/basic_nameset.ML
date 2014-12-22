(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  Title:      basic_nameset.ML
    Author:     Lucas Dixon
                lucas.dixon@ed.ac.uk
                Oct 2005 - 2010                                        *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  DESCRIPTION:

    Used to get new nice fresh names (quickly). With a slight
    adaptation, it can also be thought of as a memory management where
    names are memory locations.

    A descrete interval table (for fresh names): a specialised, AVL
    tree inspired, representation for holding intervals and getting
    quick lookup/add/delete, worst case O(Log(n)^2); but when adjacent
    names are used it is much faster (often constant time).

*)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)

signature BASIC_NAME_SET =
sig
  type name

  datatype T 
    = Empty
    | Br of int (* depth of stuff this branch *)
            * (name * name) (* inclusive min/max range *)
            * T * T; (* subtrees with smaller than min in left and bigger than max in right *)

  exception empty_exp of string;
  exception duplicate_exp of name * T;

  val empty : T
  val single : name -> T
  val is_empty : T -> bool
  val is_singleton : T -> bool
  val tryget_singleton : T -> name option
  val get_min : T -> name option (* lowest value, log time *)
  val get_max : T -> name option (* highest value, log time *)
  val get_local_bot : T -> name option (* random middle name - constant time *)
  val get_local_top : T -> name option (* random middle name - constant time *)
  val get_first : T -> name option (* smallest name - log time *)
  val get_last : T -> name option (* largest name - log time *)
  
  (* number of elements *)
  val cardinality : T -> int

  (* fold over ranges, in order smallest to biggest *)
  val fold_ranges : ((name * name) -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_ranges_rev : ((name * name) -> 'a -> 'a) -> T -> 'a -> 'a
  (* fold all elements, in order, smallest to biggest *)
  val fold : (name -> 'a -> 'a) -> T -> 'a -> 'a
  val fold_rev : (name -> 'a -> 'a) -> T -> 'a -> 'a
  (* fold try, stop early if needed *)
  (* val fold_try : (name -> 'a -> 'a option) -> T -> 'a -> 'a option *)

  (* to and from lists *)
  val add_to_rangelist : T -> (name * name) list -> (name * name) list
  val list_of : T -> name list
  val of_list : name list -> T
  
  (* to seq *)
  val seq_of : T -> name Seq.seq

  (* make a fresh name, based on a given name *)
  val new :  T -> name -> name

  (* adding a name *)
  val try_ins_fresh : name -> T -> T option (* NONE is it already exists *)
  val add_new : name -> T -> (name * T)
  val ins_fresh : name -> T -> T (* raises duplicate_exp *)
  val add_new_opt : name -> T -> (name option * T) (* if name clash then SOME new-name *)
  val add : name -> T -> T (* = union1 *)

  (* adding many names *)
  val add_list : name list -> T -> T
  val add_range : name * name -> T -> T

  (* renaming *)
  val rename1 : name -> name -> T -> (name * T) (* fst disappears, adds snd *)
  val rename1_opt : name -> name -> T -> (name option * T) (* SOME if renamed to avoid a clash *)
  val try_rename1 : name -> name -> T -> T option (* NONE on name clash *)

  (* lookup *)
  val lookup : T -> name -> (name * name) option
  val contains : T -> name -> bool

  (* delete *)
  val delete' : name -> T -> T (* raises empty_exp if name not in set *)
  val delete : name -> T -> T (* is identify if name not in set *)
  val try_delete : name -> T -> T option (* is NONE if name not in set  *)
  
  (* filter; function true to keep element *)
  val filter : (name -> bool) -> T -> T

  (* val delete1 : name -> T -> T  for debugging only *)

  val contains_range : T -> (name * name) -> bool
  val contains_range_exactly : T -> (name * name) -> bool

  (* contains exactly the same ranges *)
  val eq : T -> T -> bool
  (* EQUAL = eq, LESS = snd has smallet disjoint member, 
     GREATER = fst has smallest disjoint member *)
  val ord : T * T -> order

  (* more set-like ops *)
  val union_merge : T -> T -> T (* union *)
  (* ADD: add a merge_distinct which raises exception on not_distinct? *)
  val subtract : T -> T -> T (* subtraction = (fst - snd) *)
  val remove_set : T -> T -> T (* subtraction = (snd - fst) *)
  val intersect : T -> T -> T (* intersection *)
  val contrast : T -> T -> (T * T * T) (* in only left, in both, in only right*)

  (* set predicates *)
  val nonempty_intersect : T -> T -> bool (* intersection is not empty *)


  (* checking properties of all names *)
  val forall : (name -> bool) -> T -> bool
  val exists : (name -> bool) -> T -> bool
  val get_exists : (name -> bool) -> T -> name option
  val search : (name -> 'a option) -> T -> 'a option

  (* moving around the name set (log time) *)
  val next_smaller : T -> name -> name option
  val next_bigger : T -> name -> name option

  (* pull out the biggest and smallest elements (log time) *)
(*   val pull_biggest : T -> (name * T) option;
     val pull_smallest : T -> (name * T) option; *)

  (* pull out the biggest and smallest range elements (log time) *)
  val pull_biggest_range : T -> ((name * name) * T) option; 
  val pull_smallest_range: T -> ((name * name) * T) option;
  (* "empty_exp str" exception throwing versions.. *)
  val pull_biggest_range' : T -> (name * name) * T;
  val pull_smallest_range': T -> (name * name) * T;

  (* pull from the middle range, as value (either the top or bot end)
     out from the name table (constant time), give back element and
     new table; NONE if empty; *)
  val pull_local_top : T -> (name * T) option;
  val pull_local_bot : T -> (name * T) option;

  val del_local_top : T -> T;
  val del_local_bot : T -> T;

  (* make the list of every element pulled out and paired with the rest:
     [1,2,3] => [(1,[2,3]), (2,[1,3]), (3,[1,2])]                     *)
  val pull_each : T -> (name * T) list

  (* check subset relation: true iff first set is a subset of the second set. *)
  val sub_set : T -> T -> bool

  (* the list of all subsets *)
  val powerset : T -> T list

  (* assumes a in tr; returns { ((leq a tr) UN x) | a <= x & x in tr }, 
     where (leq a tr) is elements less than or equal to a in tr. *) 
  val powerset_geq : name -> T -> T list

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
  val find_and_delete_bot_join : name * name -> T -> name * T
  val find_and_delete_top_join : name * name -> T -> name * T

  val pretty_full : T -> Pretty.T
  val print_full : T -> unit
  val pretty_struct : T -> Pretty.T
  val print_struct : T -> unit
  val pretty : T -> Pretty.T
  val print : T -> unit

  val assert_correct : T -> T (* raises if not correct *)
  
  val subdivide : int -> T -> T list (* subdivide into N roughly even sets *)


  (* should never happen *)
  exception delete_exp of string * name * T;
  exception add_range_exp of string * (name * name) * T;
  exception union_merge_exp of string * T * T;

  (* FIXME: unify the various bug excepotions *)
  exception bug0_exp of string;
  exception bug_exp of string * T;
  exception bug2_exp of string * T * T;
  exception bad_fnametab_exp of string * T;

end;


(* Fresh Name Table Functor *)
functor BasicNameSetFun(N : NAMER)
(*  : BASIC_NAME_SET *)
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
          * (name * name) (* inclusive min/max range *)
          * T * T; (* subtrees with smaller than min in left and bigger than max in right *)

(* hopefully impossible exceptions for debugging *)
exception delete_exp of string * name * T;
exception add_range_exp of string * (name * name) * T;
exception add_exp of string * name * T;
exception union_merge_exp of string * T * T;

          
(* hopefully impossible exceptions *)
exception bug0_exp of string;
exception bug_exp of string * T;
exception bug2_exp of string * T * T;
exception duplicate_exp of name * T;

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


(* basic constructors *)
val empty = Empty;
fun single x = Br(1,(x,x),Empty,Empty);

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
fun add_to_rangelist Empty L = L
  | add_to_rangelist (Br (_, g, l, r)) L =
    add_to_rangelist l (g :: add_to_rangelist r L);

fun dest_range (b,t) = 
    (case N.ord (b, t) 
      of EQUAL => [b]
       | GREATER => [] (* should not happen - new exception? *)
       | LESS => b :: (dest_range (N.suc b, t)));

(* give back list of all used names, expands the intervals *)
fun list_of tr = maps dest_range (add_to_rangelist tr []);

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
  | get_last (Br (_, (b,t),l,r)) = get_last r
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
fun pull_biggest_range' Empty = raise empty_exp "pull_biggest"
  | pull_biggest_range' (Br(d,v,l,Empty)) = (v, l)
  | pull_biggest_range'  (Br(d,v,l,r)) = 
    let val (biggest,newr) = pull_biggest_range'  r
    in (biggest, rebalance (Br(br_depth l newr,v,l,newr))) end
fun pull_biggest_range tr = 
    SOME (pull_biggest_range' tr) handle empty_exp _ => NONE;

fun pull_smallest_range' Empty = raise empty_exp "pull_smallest"
  | pull_smallest_range' (Br(d,v,Empty,r)) = (v, r)
  | pull_smallest_range' (Br(d,v,l,r)) = 
    let val (smallest,newl) = pull_smallest_range' l
    in (smallest, rebalance (Br(br_depth newl r,v,newl,r))) end
fun pull_smallest_range tr = 
    SOME (pull_smallest_range' tr) handle empty_exp _ => NONE;

(* for use when these are the children of a deleted node, in order to
   make the replacement for the deleted node. *)
(* THINK: is this the most efficient? *)
fun join_from_delete Empty Empty = Empty
  | join_from_delete (tr as Br _) Empty = tr
  | join_from_delete Empty (tr as Br _) = tr
  | join_from_delete (l as Br(ld,lv,ll,lr)) (r as Br(rd,rv,rl,rr)) = 
    if ld >= rd then 
      let val (newv, newl) = pull_biggest_range' l 
      in Br(br_depth newl r, newv, newl, r) end
    else 
      let val (newv, newr) = pull_smallest_range' r 
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


fun add s = add' (pred_or_same s, s, N.suc s);
(* fun add s = assert_correct o add' (pred_or_same s, s, N.suc s); *)

fun add_list l = fold add l;
(* fun of_list l = assert_correct (add_list l empty); *)
fun of_list l = add_list l empty;

(* IMPROVE: implement efficient version, cached and/or computer from intervals *)
val cardinality = length o list_of;


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

(* give back a lazy list of all used names *)
fun seq_of tr = let fun proceed  NONE    = Seq.empty
                      | proceed (SOME n) = Seq.make (
                          fn()=>SOME (n,(proceed (next_bigger tr n)))
                        )
                in proceed (get_min tr) end;
    
(* adding new elements *)
fun add_new a tr = 
    case lookup tr a of 
      NONE => (a, add a tr)
    | SOME (b,t) => 
      let val a2 = N.suc t in (a2, add a2 tr) end;

fun add_new_opt a tr = 
    case lookup tr a of 
      NONE => (NONE, add a tr)
    | SOME (b,t) => 
      let val a2 = N.suc t in (SOME a2, add a2 tr) end;

fun new tr a = 
    case lookup tr a of 
      NONE => a
    | SOME (b,t) => 
      let val a2 = N.suc t in a2 end;

fun ins_fresh a tr = 
    case lookup tr a of 
      NONE => add a tr
    | _ => raise duplicate_exp (a,tr);

fun try_ins_fresh a tr = 
    case lookup tr a of 
      NONE => SOME (add a tr) (* fresh, so add it *)
    | SOME _ => NONE; (* not fresh! *)


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

(* with rebalancing *)
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
                     | _ => Br (1 + Int.max(depth_of l',depth_of r),(b',t2),l',r))
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
(* val union_merge = assert_correct oo union_merge; *)


fun drop_right (Br(_,(b,t),l,_)) = Br(1+(depth_of l),(b,t),l,Empty)
  | drop_right _ = raise bug0_exp "Tried to drop right from Empty tree";
fun drop_left  (Br(_,(b,t),_,r)) = Br(1+(depth_of r),(b,t),Empty,r)
  | drop_left _ = raise bug0_exp "Tried to drop left from Empty tree";

(* fast(est?) sub_set check arg1 is a subset of arg2 *)
fun sub_set Empty tr = true
  | sub_set (Br _) Empty = false
  | sub_set (n1 as Br(d1,(b1,t1),l1,r1)) (n2 as Br(d2,(b2,t2),l2,r2)) = 
    (case N.ord (t1,b2) 
      of LESS => sub_set (drop_right n1) l2 andalso sub_set r1 n2
       | EQUAL => (case N.ord (b1, b2) 
                    of EQUAL => sub_set l1 l2 andalso sub_set r1 (drop_left n2)
                     | _ => false)
       | GREATER =>
         (case N.ord (b1,t2)
           of LESS => (case N.ord (b1,b2) 
                         of LESS => false
                          | EQUAL => (case N.ord (t1,t2)
                                        of GREATER => false
                                         | EQUAL => sub_set l1 l2 andalso sub_set r1 r2
                                         | LESS => sub_set l1 l2 andalso sub_set r1 (drop_left n2))
                          | GREATER => (case N.ord (t1,t2)
                                          of GREATER => false
                                           | EQUAL => sub_set l1 (drop_right n2) andalso sub_set r1 r2
                                           | LESS => sub_set l1 (drop_right n2) andalso sub_set r1 (drop_left n2)))
            | EQUAL =>
              (case N.ord (t1, t2) 
                of EQUAL => sub_set l1 (drop_right n2) andalso sub_set r1 r2
                 | _ => false)
            | GREATER => sub_set (drop_left n1) r2 andalso sub_set l1 n2));

(* assumes b < t *)
fun contains_range tr (r as (b,t)) = 
    let fun recf Empty = false
          | recf (tr as Br(d1,(b1,t1),l1,r1)) = 
            (case N.ord (t,b1) of 
               LESS => recf l1 
             | EQUAL => (case N.ord (b, b1) of EQUAL => true 
                         (* r has value in gap between t1 and l1 *)
                                             | _ => false) 
             | GREATER => (case N.ord (b,t1) of GREATER => recf r1
                           | EQUAL => (case N.ord (t,t1) of EQUAL => true
                                        (* r has value in gap between t1 and r1 *)
                                                          | _ => false)
                           | LESS => ((* r has value in gap between t1 and r1 *)
                                      case N.ord (t,t1) of GREATER => false
                                                          | _ => true)))
    in recf tr end;

(* assumes b < t *)
fun contains_range_exactly tr (r as (b,t)) = 
    let fun recf Empty = false
          | recf (tr as Br(d1,(b1,t1),l1,r1)) = 
            (case N.ord (t,t1) of 
               LESS => 
               (case N.ord (t, b1) of LESS => recf l1 
                                    | _ => false)
             | EQUAL => (case N.ord (b, b1) of EQUAL => true 
                         (* r has value in gap between t1 and l1 *)
                                             | _ => false)
             | GREATER => (case N.ord (b, t1) of GREATER => recf r1
                                               | _ => false))
    in recf tr end;




(* delete - consider cases: 1. s is in the middle of a range, then we
need to split the range and insert the appropriate half into the
shorter branch of the tree. 2. s is the end of some range, then we
just move the range along one bit. But when the range we are adjusting
is just one element, then we simply remove this node, as per avl
trees. 3. recurse on left or right *)
fun delete' s Empty = raise empty_exp "delete': no such element to delete"
  | delete' s (tr as Br (d,v as (b,t),l,r)) = 
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
         let val newl = delete' s l
         in rebalance (Br (br_depth newl r,v,newl,r)) end
       | (GREATER, GREATER) => 
         let val newr = delete' s r
         in rebalance (Br (br_depth l newr,v,l,newr)) end
       | (EQUAL, GREATER) => raise bug0_exp "impossible! delete' 1"
       | (LESS, EQUAL) => raise bug0_exp "impossible! delete' 2"
       | (LESS, GREATER) => raise bug0_exp "impossible! delete' 3");

fun delete el set = delete' el set handle empty_exp _ => set;
fun try_delete el set = SOME (delete' el set) handle empty_exp _ => NONE;

(* val delete1 = delete;
fun delete2 s t = 
    assert_correct (delete1 s t)
    handle bad_fnametab_exp (msg,_) => raise delete_exp ("delete2 bug: " ^ msg, s, t)
         | bug0_exp msg => raise delete_exp ("delete2 bug: " ^ msg, s, t);
val delete = delete2;
*)

(* pulls out the biggest element, also giving back new balanced tree,
   assumes initial true was well formed (especially balanced). *)
fun pull_local_top Empty = NONE
  | pull_local_top (tr as (Br(_,(b,t),_,_))) = SOME (t, delete t tr);

fun pull_local_bot Empty = NONE
  | pull_local_bot (tr as Br(_,(b,t),_,_)) = SOME (b, delete b tr);

fun del_local_top Empty = Empty
  | del_local_top (tr as (Br(_,(b,t),_,_))) = delete t tr;

fun del_local_bot Empty = Empty
  | del_local_bot (tr as Br(_,(b,t),_,_)) = delete b tr;
    

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

(* returns powerset of tr as a list *)
fun powerset tr = 
    (case get_first tr of 
       NONE => []
     | SOME a => powerset_geq a tr)


(* fold over the ranges in the name tree, in order smallest to biggest *)
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

(* IMPROVE?: write nice optimal version based on top-level
   rotatability, using the depth info. should be: log(tr1), currently: min(lg(tr1)^2) *)
fun ord (tr1, tr2) = 
    (case (pull_smallest_range tr1, pull_smallest_range tr2)
      of (NONE, NONE) => EQUAL (* both empty *)
       | (NONE, SOME _) => LESS
       | (SOME _, NONE) => GREATER
       | (SOME ((b1,t1),tr1'), SOME ((b2,t2),tr2')) => 
         (case (N.ord (b1,b2)) 
           of LESS => GREATER
            | GREATER => LESS
            | EQUAL => (case (N.ord (t1,t2)) 
                         of LESS => LESS
                          | GREATER => GREATER
                          | EQUAL => ord (tr1', tr2'))));

fun eq tr1 tr2 = case ord (tr1,tr2) of EQUAL => true | _ => false;


(* IMPROVE: THIS IS NOT EFFICIENT! *)
(* subtract 1st from 2nd = fromthis - subtractthis *)
fun subtract fromthis subtractthis =  
    fold delete (list_of subtractthis) fromthis;
(* val subtract = assert_correct oo subtract; *)
(* same as above but swaped arguments for curried folding *)
fun remove_set subtractthis fromthis =  
    fold delete (list_of subtractthis) fromthis;
(* val remove_set = assert_correct oo remove_set; *)

(* IMPROVE: this is the inefficient version; write the efficient one based on
   the intervals ! *)
fun intersect A B = 
    fold (fn a => if contains B a then I else delete a) (list_of A) A;


(* IMPROVE: this is the inefficient version; write the efficient one based on
   the intervals ! *)
fun nonempty_intersect A B = 
    let exception stop of unit
    in
      (fold (fn a => fn () => 
            if contains B a then raise stop () else ()) (list_of A) (); false)
      handle stop () => true
    end;

(* contrast: compute leftonly tree, intersection and
   right-only tree. IMPROVE: make efficient version. *)
fun contrast lt rt = 
    (subtract lt rt, intersect lt rt, subtract rt lt);


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

(* renaming *)
fun rename1 n1 n2 tr = 
    (case try_delete n1 tr 
      of NONE => (n2,tr)
       | SOME tr2 => add_new n2 tr2)

fun rename1_opt n1 n2 tr = (* optional new name if n2 clashes *)
    (case try_delete n1 tr 
      of NONE => (NONE,tr)
       | SOME tr2 => add_new_opt n2 tr2)

fun try_rename1 n1 n2 tr = (* optional result: SOME if no name clash *)
    (case try_delete n1 tr 
      of NONE => SOME tr
       | SOME tr2 => 
         try_ins_fresh n2 tr2);


(* fold over all elements *)
fun fold_try f [] a = a
  | fold_try f (h::t) a = 
    (case f h a of NONE => NONE
     | SOME a2 => fold_try f t a2);

fun fold f = fold_ranges (Basics.fold f o dest_range);
fun fold_rev f = fold_ranges_rev (Basics.fold_rev f o dest_range);

fun filter f s = fold (fn e => if (f e) then I else delete e) s s;

exception found_exp of name;

fun exists f ns = 
    (fold (fn n => fn () => if (f n) then raise found_exp n else ()) ns (); 
     false)
    handle found_exp _ => true;
fun get_exists f ns = 
    (fold (fn n => fn () => if (f n) then raise found_exp n else ()) ns (); 
     NONE)
    handle found_exp n => SOME n;

fun search f ns = 
    let exception found_data of 'a in
      (fold (fn n => fn () => 
           case (f n) 
           of NONE => () 
            | SOME x => raise found_data x)
          ns (); NONE)
    handle found_data r => SOME r
    end;

fun forall f ns = 
    (fold (fn n => fn () => if (f n) then () else raise found_exp n) ns (); 
     true)
    handle found_exp _ => false;


(* {a,b,c,d} => [(a, {b,c,d}), (b, {a,c,d}), (c,{a,b,d}), ... ] *)
fun pull_each nset = fold (fn n => fn l => (n,delete n nset)::l) nset [];


fun subdivide n nset = let
  val size = ((cardinality nset) div n)
  fun subdiv []   _ = []
    | subdiv rest 0 = [of_list rest]
    | subdiv rest i = let val (l1,l2) = chop size rest
                      in (of_list l1)::(subdiv l2 (i-1))
                      end
in subdiv (list_of nset) (n-1)
end

(*
exception stop_exp of unit;
( * NOTE: not used;
  IMPROVE: provide more efficient set based 2obj fold * )
fun fold_2obj_names f (obj1,obj2) a = 
   (SOME o snd ooo fold)
   (fn n1 => 
    (fn ([], a') => raise stop_exp ()
      | (n2::Ns, a') => (Ns,f (n1,n2) a'))) 
    obj1 (list_of obj2, a)
    handle stop_exp _ => NONE
*)

end; (* structure *)


