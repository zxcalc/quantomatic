(* Generic testing *)
val tests_failed = Unsynchronized.ref ([] : (int * exn) list);
val test_num = Unsynchronized.ref 0;
fun new_test f () = 
    (test_num := (!test_num) + 1; 
     (* do something with the current compiler-file line number? 
        (for quick knowledfe of which test raised an exception...) *)
     writeln ("Running Test: " ^ (Int.toString (!test_num )));
     (f ()) handle e => 
            (tests_failed := ((!test_num, e) :: (!tests_failed)); ())
    ); 
exception failed_tests_exp of (int * exn) list;
fun assert_no_failed_tests () = 
    (if null (!tests_failed) then () else 
    raise failed_tests_exp (!tests_failed));

