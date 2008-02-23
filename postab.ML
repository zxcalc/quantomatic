(* position tables: abstract orderings for things which can easily be
updated. The 'value' is the objects position in the (sufficiently)
balanced binary tree. The path info gives the ordering-value for the
element. We have log insert and delete operations, etc. *)
signature POS_TAB 
= sig

exception incomparible_poses_exp of string;
exception bad_ptab_exp of string;
exception bad_seq_exp of string;
exception empty_ptab_exp of string;

type pos = bool list;

val compare_pos : pos * pos -> General.order

type 'a T;

val empty : 'a T;
val depth : 'a T -> int;
val lfseq_of : 'a T -> 'a Seq.seq;
val fold_lfs : ('a -> 'b -> 'b) -> 'a T -> 'b -> 'b;
val replace_lfs : 'b Seq.seq -> 'a T -> 'b T;
val insert_anywhere : 'a -> 'a T -> (pos * 'a T);
val delete_anywhere : 'a -> 'a T -> ('a * 'a T);

end;

structure PosTab : POS_TAB = 
struct

(* positions in the binary tree - these are the ordering info *)
type pos = bool list;

exception incomparible_poses_exp of string;

(* (false = 0 = left) < (true = 1 = right), with lex order on list of bools *)
fun compare_pos ([], []) = EQUAL
  | compare_pos (true::t, true::t2) = compare_pos t t2
  | compare_pos (false::t, false::t2) = compare_pos t t2
  | compare_pos (true::_, false::_) = GREATER
  | compare_pos (false::_, true::_) = LESS
  | compare_pos (_, _) = raise incomparible_poses_exp "compare_pos";

(* invarient: ptab should be balanced and names relate to names in the
TNSet *)
datatype 'a ptab = Lf of 'a
                 | Br of int * 'a ptab * 'a ptab; (* depth, left, right *)

type 'a T = 'a ptab option;

val empty = (NONE : 'a T);

fun depth_of_ptab (Lf _) = 0
  | depth_of_ptab (Br (d,l,r)) = d;

fun depth NONE = 0
  | depth (SOME ptab) = depth_of_ptab ptab;

(* insert into smallest branch, returns position *)
fun insert_anywhere' n (Br (i, l, r)) = 
    (case Int.compare (depth_of_ptab l, depth_of_ptab r) 
      of LESS => 
         let val (p2,l2) = insert_anywhere' n l
         in (false::p2,Br(i,l2,r)) end
       | GREATER => 
         let val (p2,r2) = insert_anywhere' n r
         in (true::p2,Br(i,l,r2)) end
       | EQUAL => 
         let val (p2,r2) = insert_anywhere' n r
         in (true::p2,Br(i+1,l,r2)) end)
  | insert_anywhere' n (Lf n2) = ([],Br(1,Lf n, Lf n2));
fun insert_anywhere n NONE = SOME ([],Lf n)
  | insert_anywhere n (SOME ptab) = SOME (insert_anywhere' n ptab);

(* fold over leafs in order *)
fun fold_ptab_lfs f (Lf x) a = (f x a)
  | fold_ptab_lfs f (Br (i,l,r)) a = fold_ptab_lfs f r (fold_ptab_lfs f l a);
fun fold_lfs f NONE a = a
  | fold_lfs f (SOME ptab) a = fold_ptab_lfs f ptab a;

(* lazy sequence construction *)
fun lfseq_of_ptab (Lf x) () = SOME (x, Seq.empty)
  | lfseq_of_ptab (Br (i,l,r)) () = 
    (case lfseq_of_ptab l ()
      of NONE => (lfseq_of_ptab r ())
       | SOME (x,seq) => SOME(x, Seq.append seq (Seq.make (lfseq_of_ptab r))));
fun lfseq_of NONE = Seq.empty
  | lfseq_of (SOME ptab) = Seq.make (lfseq_of_ptab ptab);

exception bad_ptab_exp of string;
exception bad_seq_exp of string;


(* given a sequence, use it to replace named in the ptab *)
fun replace_ptab_lfs seq (Lf n) = 
    (case Seq.pull seq of 
       NONE => raise bad_seq_exp "replace_names: run out of elements"
     | SOME (n2, seq2) => (Lf n2, seq2))
  | replace_ptab_lfs seq (Br (i,l,r)) = 
    let 
      val (l2,seq2) = replace_ptab_lfs seq l
      val (r2,seq3) = replace_ptab_lfs seq2 r
    in (Br(i,l2,r2), seq3) end;

fun replace_lfs seq NONE = NONE
  | replace_lfs seq (SOME ptab) = SOME (fst (replace_ptab_lfs seq ptab));

(* remove from biggest branch *)
(* returns name of deleted element and optional ptab, SOME if it was not Lf *)
fun delete_anywhere' (Br (i, l, r)) = 
    (case Int.compare (depth_of_ptab l, depth_of_ptab r) 
      of LESS => (case delete_anywhere' r
                   of (n,SOME r') => (* right was Br *)
                      (n,SOME (Br((depth_of_ptab r) + 1,l,r')))
                    | (n,NONE) => raise bad_ptab_exp "delete_anywhere:right")
       | GREATER => (case delete_anywhere' l
                   of (n,SOME l') => (* left was Br *)
                      (n,SOME (Br((depth_of_ptab l) + 1,l',r)))
                    | (n,NONE) => raise bad_ptab_exp "delete_anywhere:left")
       | EQUAL => (case delete_anywhere' r
                    of (n,NONE) => (n,SOME l) (* right was Lf *)
                     | (n, SOME r') => (n, SOME (Br(i,l,r')))))
  | delete_anywhere' (Lf n) = (n, NONE);

exception empty_ptab_exp of string;
fun delete_anywhere NONE = raise empty_ptab_exp "delete_anywhere"
  | delete_anywhere (SOME ptab) = delete_anywhere' ptab;

end; (* struct *)
