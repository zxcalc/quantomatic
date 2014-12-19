(* TODO: re-introduce file name info in test output *)

(* Generic testing *)
signature TESTING = 
sig 
  structure Log : LOG;

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

  (* Should we run tests? *)
  val run_tests_ref : bool Unsynchronized.ref;
  (* if (!run_tests_ref) then loads the file. *)
  (*val make_test : string -> unit;*)

  exception assertion_failed_exp of string;

  (* raises assertion_failed_exp with the string arg if the bool arg is false *)
  val assert : string -> bool -> unit;
  (* raises assertion_failed_exp with the string arg if the function returns false *)
  val fassert : string -> ('a -> bool) -> 'a -> unit;

  

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


structure Log = Log(val init_level = 1);

val tests_failed_ref = Unsynchronized.ref ([] : failure list);
val test_num_ref = Unsynchronized.ref 0;
fun test testname f x = 
    let val _ = test_num_ref := (!test_num_ref) + 1;
        val test_num_str = (Int.toString (!test_num_ref));
        val file_text = case NONE (*PolyML.Project.get_current_filename()*)
                          of NONE => ""
                           | SOME s => " from file '"^s^"'"
        fun run_test () = let val result = f x in
          Log.log 1 ("[PASS] "^test_num_str^": "^testname); result
        end
    in 
      (* do something with the current compiler-file line number? 
        (for quick knowledge of which test raised an exception...) *)
      Log.log 2 ("Running test "^test_num_str^": " ^ testname ^ file_text);
      ((SOME (PolyML.exception_trace run_test)) handle e => 
             (Log.log 0 ("[FAIL] "^test_num_str^": "^testname ^": "^ (PolyML.makestring e));
              tests_failed_ref := (
                { file = NONE (*PolyML.Project.get_current_filename()*),
                  test = testname,
                  id = (!test_num_ref),
                  err_exn = e } :: (!tests_failed_ref));
              NONE))
    end;

exception failed_tests_exp of failure list;
fun assert_no_failed_tests () = 
    if null (!tests_failed_ref)
    then Log.log 1 ((Int.toString (!test_num_ref))^" tests passed so far")
    else 
      (Log.log 0 "The following tests failed:";
       map (fn t => Log.log 0 (" * '"^(#test t)^
                       (case #file t
                         of SOME f => ("' in "^f)
                          | NONE   => "")
                   ))
           (!tests_failed_ref);
       raise failed_tests_exp (!tests_failed_ref))

(* *)
val run_tests_ref = Unsynchronized.ref true;

(*fun make_test path_to_file =
    if !run_tests_ref then PolyML.Project.make path_to_file else ();*)

exception assertion_failed_exp of string;
fun assert msg true = Log.log 3 ("  - " ^ msg ^ " [OK]")
  | assert msg false = (Log.log 2 ("  - " ^ msg ^ " [FAILED]");
      raise assertion_failed_exp ("Assertion '" ^ msg ^ "' failed"))
fun fassert msg f x = assert msg (f x)
    
end;

(* small example usage: *)
(*
local 
  (* define some basic values*)
  val a = "a";
in
  (* *)
  val _ = Testing.test "the name of the first test" (fn () => let 
    (* the test code goes in here, and is assumed not to raise an exception. *)
    val _ = Testing.assert "uninteresting test" (a = "a")
    in () end) ();

  (* more tests... *)
  val _ = Testing.test "the name of another test" (fn () => let 
    val _ = Testing.assert "another uninteresting test" (not (a = "b"))
    in () end) ();

  (* This raises an exception if some test fails and prints details for all 
     failed tests *)
  val _ = Testing.assert_no_failed_tests();
end; (* ends local namespace used for testing *)
*)
