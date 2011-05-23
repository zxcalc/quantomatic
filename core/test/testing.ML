(* Generic testing *)
signature TESTING = 
sig 
  type failure = { file: string option,
                   test: string,
                   id: int,
                   err_exn : exn };

  (* errors so far *)
  val tests_failed_ref : failure list Unsynchronized.ref;

  (* The test function *)
  val test : string (* Name of test *) 
             -> ('a -> 'b) (* funtion to test *)
             -> 'a (* test on this value *)
             -> 'b option (* NONE indicates for failure *)

  (* *)
  exception failed_tests_exp of failure list;
  
  (* raises if tests_failed_ref is not empty *)
  val assert_no_failed_tests : unit -> unit;
end;

structure Testing 
: TESTING
= struct

type failure = { file: string option,
                 test: string,
                 id: int,
                 err_exn : exn };


structure Log = Log(val level_ref = Unsynchronized.ref 0);

val tests_failed_ref = Unsynchronized.ref ([] : failure list);
val test_num_ref = Unsynchronized.ref 0;
fun test testname f x = 
    let val _ = test_num_ref := (!test_num_ref) + 1;
        val test_num_str = (Int.toString (!test_num_ref));
    in 
      (* do something with the current compiler-file line number? 
        (for quick knowledfe of which test raised an exception...) *)
      Log.log 0 ("From file: " ^ (PolyML.Project.string_of_current_filename()));
      Log.log 0 ("Running Test: '" ^ testname ^ "' (" ^test_num_str^ ")");
      SOME (f x) handle e => 
             (Log.log 0 ("Test named '"^testname^"' (" ^test_num_str
                         ^") failed :"^ (PolyML.makestring e));
              tests_failed_ref := (
                { file = PolyML.Project.get_current_filename(),
                  test = testname,
                  id = (!test_num_ref),
                  err_exn = e } :: (!tests_failed_ref));
              NONE)
    end;

exception failed_tests_exp of failure list;
fun assert_no_failed_tests () = 
    (if null (!tests_failed_ref) then () else 
    raise failed_tests_exp (!tests_failed_ref));

end;
