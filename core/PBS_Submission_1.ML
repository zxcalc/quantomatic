use "ROOT.ML";
(*Force the correct heap... we run into many errors otherwise...*)
PolyML.Project.ensure_at_latest_heap ();
(*Limit to 12 cores*)
val _ = Multithreading.max_threads := 24;
(*Fun the relevant test...*)
(*hardness_test ordermatters branches depth hardnesses who_runs*)
(*open MJB_Par_Tree_Speed_Test.Tests;
val _ = run_constant ();*)
(*
MJB_Test_Matching_Speed.test_greedy 7 0; (*The weird behaviour test!*)
*)
(*
open MJB_Par_Seq_Speed_Test;
open Tests;
val _ = run_overload_test();
*)

(*The bugged behaviour*)
open MJB_Test_Matching_Speed;
val _ = test 30;
