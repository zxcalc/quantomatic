(* Generic testing *)
structure Testing = 
struct

structure Log = Log(val level_ref = Unsynchronized.ref 0);

val tests_failed_ref = Unsynchronized.ref ([] : (int * exn) list);
val test_num_ref = Unsynchronized.ref 0;
fun new_test f () = 
    (test_num_ref := (!test_num_ref) + 1; 
     (* do something with the current compiler-file line number? 
        (for quick knowledfe of which test raised an exception...) *)
     Log.log 0 ("Running Test: " ^ (Int.toString (!test_num_ref )));
     (f ()) handle e => 
            (tests_failed_ref := ((!test_num_ref, e) :: (!tests_failed_ref)); ())
    ); 

exception failed_tests_exp of (int * exn) list;
fun assert_no_failed_tests () = 
    (if null (!tests_failed_ref) then () else 
    raise failed_tests_exp (!tests_failed_ref));

end;
