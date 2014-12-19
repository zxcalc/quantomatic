(*  Title:      Pure/Isar/proof_node.ML
    Author:     Makarius

Proof nodes with linear position and backtracking.
*)

signature PROOF_NODE =
sig
  type T
  val init: Proof.state -> T
  val current: T -> Proof.state
  val position: T -> int
  val back: T -> T
  val applys: (Proof.state -> Proof.state Seq.result Seq.seq) -> T -> T
  val apply: (Proof.state -> Proof.state) -> T -> T
end;

structure Proof_Node: PROOF_NODE =
struct

(* datatype *)

datatype T = Proof_Node of
  (Proof.state *                (*first result*)
   Proof.state Seq.seq) *       (*alternative results*)
  int;                          (*linear proof position*)

fun init st = Proof_Node ((st, Seq.empty), 0);

fun current (Proof_Node ((st, _), _)) = st;
fun position (Proof_Node (_, n)) = n;


(* backtracking *)

fun back (Proof_Node ((_, stq), n)) =
  (case Seq.pull stq of
    NONE => error "back: no alternatives"
  | SOME res => Proof_Node (res, n));


(* apply transformer *)

fun applys f (Proof_Node ((st, _), n)) =
  Proof_Node (Seq.first_result "Empty result sequence -- proof command failed" (f st), n + 1);

fun apply f = applys (Seq.single o Seq.Result o f);

end;
