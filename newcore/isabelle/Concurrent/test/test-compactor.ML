local
  (*************************************************)
  (****************CORRECTNESS TESTS****************)
  (*************************************************)
  
  (*Test setting the group size in a static compactor*)
  val _ = Testing.test "Compactor: static group size set test" (fn () =>
    let
      val comp = Compactor.new_static 100;
      val _ = if ((Compactor.get_size comp)=100) then () else
              raise ERROR "Compactor: static size set failure 1";
      val comp = Compactor.set_size comp 50;
      val _ = if ((Compactor.get_size comp)=50) then () else
              raise ERROR "Compactor: static size set failure 2";
    in
      ()
    end) ()
    
  (*Test setting the time in a dynamic compactor*)
  val _ = Testing.test "Compactor: dynamic time set test" (fn () =>
    let
      val comp = Compactor.new_dynamic (Time.fromSeconds 10);
      val _ = if ((Time.toSeconds(Compactor.get_time comp))=10) then () else
              raise ERROR "Compactor: dynamic time set failure 1";
      val comp = Compactor.set_time comp (Time.fromSeconds 20);
      val _ = if ((Time.toSeconds(Compactor.get_time comp))=20) then () else
              raise ERROR "Compactor: dynamic time set failure 2";
    in
      ()
    end) ()
    
  (*Test the type of a compactor*)
  val _ = Testing.test "Compactor: testing type" (fn () =>
    let
      val comp = Compactor.new_dynamic (Time.fromSeconds 10);
      val _ = if (Compactor.is_dynamic comp) then () else
              raise ERROR "Compactor: dynamic type failure 1";
      val _ = if (not (Compactor.is_static comp)) then () else
              raise ERROR "Compactor: dynamic type failure 2";
      val comp = Compactor.new_static 100;
      val _ = if (Compactor.is_static comp) then () else
              raise ERROR "Compactor: static type failure 1";
      val _ = if (not (Compactor.is_dynamic comp)) then () else
              raise ERROR "Compactor: static type failure 2";
    in
      ()
    end) ()
    
  (*Test setting the estimate for a dynamic compactor*)
  val _ = Testing.test "Compactor: dynamic estimate set" (fn () =>
    let
      val comp = Compactor.new_dynamic (Time.fromSeconds 10);
      (*Assume nothing about the default scenario
      as this may be subject to change*)
      val comp = Compactor.set_estimated_number comp NONE;
      val _ = if (is_none (Compactor.get_estimated_number comp)) then () else
              raise ERROR "Compactor: dynamic estimate set failure 1";
      val comp = Compactor.set_estimated_number comp (SOME 100);
      val _ = if (not (is_none (Compactor.get_estimated_number comp))) then
              if ((the (Compactor.get_estimated_number comp))=100) then () else
              raise ERROR "Compactor: dynamic estimate set failure 2" else
              raise ERROR "Compactor: dynamic estimate set failure 3";
    in
      ()
    end) ()
    
  (*Test setting the fold direction (this test does not see if folding worked)*)
  val _ = Testing.test "Compactor: folding direction test" (fn () =>
    let
      (*Static case*)
      val comp = Compactor.new_static 100;
      (*Again, do not assume a default since this may change*)
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      val _ = if (Compactor.Left=(Compactor.get_fold_direction comp)) then () else
              raise ERROR "Compactor: static direction set failure 1";
      val comp = Compactor.set_fold_direction comp Compactor.Right;
      val _ = if (Compactor.Right=(Compactor.get_fold_direction comp)) then () else
              raise ERROR "Compactor: static direction set failure 2";
      (*Dynamic case*)
      val comp = Compactor.new_dynamic (Time.fromSeconds 10);
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      val _ = if (Compactor.Left=(Compactor.get_fold_direction comp)) then () else
              raise ERROR "Compactor: dynamic direction set failure 1";
      val comp = Compactor.set_fold_direction comp Compactor.Right;
      val _ = if (Compactor.Right=(Compactor.get_fold_direction comp)) then () else
              raise ERROR "Compactor: dynamic direction set failure 2";
    in
      ()
    end) ()
  
  (*Test the priming of a dynamic compactor. There are a lot of combinations here*)
  val _ = Testing.test "Compactor dynamic priming set test" (fn () =>
    let
      val comp = Compactor.new_dynamic (Time.fromSeconds 10);
      (*Start by checking when it is set not to prime...*)
      val comp = Compactor.do_not_prime comp;
      val _ = if (is_none (Compactor.check_prime comp)) then () else
              raise ERROR "Compactor: dynamic prime set failure 1";
      (*Check the prime status when it should become static...*)
      (*With a limit*)
      val comp = Compactor.prime comp (SOME 10) true;
      val _ = if (not (is_none (Compactor.check_prime comp))) then
              if (snd (the (Compactor.check_prime comp))) then
              if (not (is_none (fst (the(Compactor.check_prime comp))))) then
              if ((the (fst (the(Compactor.check_prime comp))))=10) then () else
              raise ERROR "Compactor: dynamic prime set failure 2" else
              raise ERROR "Compactor: dynamic prime set failure 3" else
              raise ERROR "Compactor: dynamic prime set failure 4" else
              raise ERROR "Compactor: dynamic prime set failure 5";
      (*Without a limit*)
      val comp = Compactor.prime comp NONE true;
      val _ = if (not (is_none (Compactor.check_prime comp))) then
              if (snd (the (Compactor.check_prime comp))) then
              if (is_none (fst (the(Compactor.check_prime comp)))) then () else
              raise ERROR "Compactor: dynamic prime set failure 6" else
              raise ERROR "Compactor: dynamic prime set failure 7" else
              raise ERROR "Compactor: dynamic prime set failure 8";
      (*Check the prime status when it should NOT become static...*)
      (*With a limit*)
      val comp = Compactor.prime comp (SOME 10) false;
      val _ = if (not (is_none (Compactor.check_prime comp))) then
              if (not (snd (the (Compactor.check_prime comp)))) then
              if (not (is_none (fst (the(Compactor.check_prime comp))))) then
              if ((the (fst (the(Compactor.check_prime comp))))=10) then () else
              raise ERROR "Compactor: dynamic prime set failure 9" else
              raise ERROR "Compactor: dynamic prime set failure 10" else
              raise ERROR "Compactor: dynamic prime set failure 11" else
              raise ERROR "Compactor: dynamic prime set failure 12";
      (*Without a limit*)
      val comp = Compactor.prime comp NONE false;
      val _ = if (not (is_none (Compactor.check_prime comp))) then
              if (not (snd (the (Compactor.check_prime comp)))) then
              if (is_none (fst (the(Compactor.check_prime comp)))) then () else
              raise ERROR "Compactor: dynamic prime set failure 13" else
              raise ERROR "Compactor: dynamic prime set failure 14" else
              raise ERROR "Compactor: dynamic prime set failure 15";
    in
      ()
    end) ()
    
  (*Test setting the scaling in a compactor *)
  val _ = Testing.test "Compactor: scaling test" (fn () =>
    let
      val comp = Compactor.new_dynamic (Time.fromSeconds 10);
      val comp = Compactor.set_scaling comp (10,100);
      val _ = if ((Compactor.get_scaling comp)=(10,100)) then () else
              raise ERROR "Compactor: scaling set failure 1";
      val comp = Compactor.set_scaling comp (5,2);
      val _ = if ((Compactor.get_scaling comp)=(5,2)) then () else
              raise ERROR "Compactor: scaling set failure 2";
    in
      ()
    end) ()
  
  (*This function generates all combinations of a priming a dynamic compactor.
  This does not change the fold direction or the target time*)
  fun dynamic_combs comp =
    [
    Compactor.prime comp NONE true, (*no limit - go static*)
    Compactor.prime comp NONE false, (*no limit - do not go static*)
    Compactor.prime comp (SOME 10) true, (*limit - go static*)
    Compactor.prime comp (SOME 10) false, (*limit - do not go static*)
    Compactor.do_not_prime comp (*do not prime at all*)
    ]
  
  (*
  NOTE: only the higher level functions for the compactor are tested for
  correctness. These use the lower level "compact" function so it is assumed
  that if these pass then that function must be working correctly.
  *)
  
  (*Generate a list of length n consisting of the element specified only*)
  fun repeat_elem 0 _ = []
    | repeat_elem n x = x::(repeat_elem (n-1) x)
  
  (*Test compacting with a sequence*)
  val _ = Testing.test "Compactor: testing compacting with a sequence" (fn () =>
    let
      (*Functions to be fed*)
      fun comp_func x = x+1;
      val func_seq = Seq.of_list (repeat_elem 100 comp_func);
      (*Test the static compactor...*)
      val comp = Compactor.new_static 5;
      (*Compact in order*)
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      val res = List.map (fn f => f()) (Seq.list_of (Seq.map (fn (x,_) => x) (Compactor.compact_with_seq comp func_seq 0)));
      val _ = if (res=(repeat_elem 20 5)) then () else
              raise ERROR "Compactor: compact with seq failure 1";
      (*Test the static compactor, overflowing the first group*)
      val comp = Compactor.new_static 200;
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      val res = List.map (fn f => f()) (Seq.list_of (Seq.map (fn (x,_) => x) (Compactor.compact_with_seq comp func_seq 0)));
      val _ = if (res=[100]) then () else
              raise ERROR "Compactor: compact with seq failure 2";
      (*Test the dynamic compactor. I have to combine the results to make sense
      of this, so we add everything up*)
      fun sum [] = 0
        | sum (x::xs) = x + (sum xs)
      (*Setup the initial dynamic compactor...*)
      val comp = Compactor.new_dynamic (Time.fromMilliseconds 10);
      (*Compute in order...*)
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      (*Get all of the combinations*)
      val comps = dynamic_combs comp;
      (*Perform all of the tests*)
      fun test_all [] _ = ()
        | test_all (c::cs) n = (
          let
            val res = sum (List.map (fn f => f()) (Seq.list_of (Seq.map (fn (x,_) => x) (Compactor.compact_with_seq c func_seq 0))));
            val _ = if (res=100) then () else
                    raise ERROR ("Compactor: compact with seq failure 3." ^ Int.toString(n));
          in
            ()
          end;test_all cs (n+1));
      val _ = test_all comps 1
    in
      ()
    end) ()
  
  (*Test compacting with a list*)
  val _ = Testing.test "Compactor: testing compacting with a list" (fn () =>
    let
      (*Functions to be fed*)
      fun comp_func x = x+1;
      val func_seq = (repeat_elem 100 comp_func);
      (*Test the static compactor...*)
      val comp = Compactor.new_static 5;
      (*Compact in order*)
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      val res = List.map (fn f => f()) (Seq.list_of (Seq.map (fn (x,_) => x) (Compactor.compact_with_list comp func_seq 0)));
      val _ = if (res=(repeat_elem 20 5)) then () else
              raise ERROR "Compactor: compact with list failure 1";
      (*Test the static compactor, overflowing the first group*)
      val comp = Compactor.new_static 200;
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      val res = List.map (fn f => f()) (Seq.list_of (Seq.map (fn (x,_) => x) (Compactor.compact_with_list comp func_seq 0)));
      val _ = if (res=[100]) then () else
              raise ERROR "Compactor: compact with list failure 2";
      (*Test the dynamic compactor. I have to combine the results to make sense
      of this, so we add everything up*)
      fun sum [] = 0
        | sum (x::xs) = x + (sum xs)
      (*Setup the initial dynamic compactor...*)
      val comp = Compactor.new_dynamic (Time.fromMilliseconds 10);
      (*Compute in order...*)
      val comp = Compactor.set_fold_direction comp Compactor.Left;
      (*Get all of the combinations*)
      val comps = dynamic_combs comp;
      (*Perform all of the tests*)
      fun test_all [] _ = ()
        | test_all (c::cs) n = (
          let
            val res = sum (List.map (fn f => f()) (Seq.list_of (Seq.map (fn (x,_) => x) (Compactor.compact_with_list c func_seq 0))));
            val _ = if (res=100) then () else
                    raise ERROR ("Compactor: compact with list failure 3." ^ Int.toString(n));
          in
            ()
          end;test_all cs (n+1));
      val _ = test_all comps 1
    in
      ()
    end) ()
  
  (*Test the compactor when mapping a function over a sequence*)
  val _ = Testing.test "Compactor: testing compactor when mapping (map) over a sequence" (fn () =>
    let
      (*The function to be mapped over the sequence*)
      fun conv 0 = "a"
        | conv _ = "b";
      (*The sequence of elements to map over...*)
      val sq = Seq.of_list ((repeat_elem 37 0)@(repeat_elem 29 1));
      (*Create the static compactor...*)
      val comp = Compactor.new_static 30;
      val res = flat (List.map (fn f => Seq.list_of (f())) (
                Seq.list_of (Compactor.compact_map_seq comp conv sq)));
      val _ = if (res=((repeat_elem 37 "a")@(repeat_elem 29 "b"))) then () else
              raise ERROR "Compactor: map over sequence failure 1";
      (*Test when overflowing the first group*)
      val comp = Compactor.new_static 100;
      val res = flat (List.map (fn f => Seq.list_of (f())) (
                Seq.list_of (Compactor.compact_map_seq comp conv sq)));
      val _ = if (res=((repeat_elem 37 "a")@(repeat_elem 29 "b"))) then () else
              raise ERROR "Compactor: map over sequence failure 2";
      (*Setup the initial dynamic compactor...*)
      val comp = Compactor.new_dynamic (Time.fromMilliseconds 10);
      (*Get all of the combinations*)
      val comps = dynamic_combs comp;
      (*Perform all of the tests*)
      fun test_all [] _ = ()
        | test_all (c::cs) n = (
          let
            val res =  flat (List.map (fn f => Seq.list_of (f())) (
                Seq.list_of (Compactor.compact_map_seq c conv sq)));
            val _ = if (res=((repeat_elem 37 "a")@(repeat_elem 29 "b"))) then () else
                    raise ERROR ("Compactor: map over sequence failure 3." ^ Int.toString(n));
          in
            ()
          end;test_all cs (n+1));
      val _ = test_all comps 1
    in
      ()
    end) ()
    
  (*Test the compactor when mapping a function over a list*)
  val _ = Testing.test "Compactor: testing compactor when mapping (map) over a list" (fn () =>
    let
      (*The function to be mapped over the list*)
      fun conv 0 = "a"
        | conv _ = "b";
      (*The list of elements to map over...*)
      val ls = (repeat_elem 37 0)@(repeat_elem 29 1);
      (*Create the static compactor...*)
      val comp = Compactor.new_static 30;
      val res = flat (List.map (fn f => (f())) (
                Seq.list_of (Compactor.compact_map_list comp conv ls)));
      val _ = if (res=((repeat_elem 37 "a")@(repeat_elem 29 "b"))) then () else
              raise ERROR "Compactor: map over list failure 1";
      (*Test when overflowing the first group*)
      val comp = Compactor.new_static 100;
      val res = flat (List.map (fn f => (f())) (
                Seq.list_of (Compactor.compact_map_list comp conv ls)));
      val _ = if (res=((repeat_elem 37 "a")@(repeat_elem 29 "b"))) then () else
              raise ERROR "Compactor: map over list failure 2";
      (*Setup the initial dynamic compactor...*)
      val comp = Compactor.new_dynamic (Time.fromMilliseconds 10);
      (*Get all of the combinations*)
      val comps = dynamic_combs comp;
      (*Perform all of the tests*)
      fun test_all [] _ = ()
        | test_all (c::cs) n = (
          let
            val res =  flat (List.map (fn f => (f())) (
                Seq.list_of (Compactor.compact_map_list c conv ls)));
            val _ = if (res=((repeat_elem 37 "a")@(repeat_elem 29 "b"))) then () else
                    raise ERROR ("Compactor: map over list failure 3." ^ Int.toString(n));
          in
            ()
          end;test_all cs (n+1));
      val _ = test_all comps 1
    in
      ()
    end) ()
    
    (*Test the compactor when performing maps over a list*)
    val _ = Testing.test "Compactor: testing compactor when mapping (maps) over a list" (fn () =>
      let
        (*The function to be mapped over the list*)
        fun conv 0 = (["a","c","d"])
          | conv _ = (["b"]);
        val rep1 = 17;
        val rep2 = 9;
        (*The list of elements to map over...*)
        val ls = (repeat_elem rep1 0)@(repeat_elem rep2 1);
        (*Create the static compactor...*)
        val comp = Compactor.new_static 5;
        val res = flat (List.map (fn f => (f())) (
                  Seq.list_of (Compactor.compact_maps_list comp conv ls)));
        val expected = (flat ((repeat_elem rep1 ["a","c","d"])@(repeat_elem rep2 ["b"])));
        val _ = if (res=expected) then () else
                raise ERROR "Compactor: maps over list failure 1";
        (*Test when overflowing the first group*)
        val comp = Compactor.new_static 100;
        val res = flat (List.map (fn f => (f())) (
                  Seq.list_of (Compactor.compact_maps_list comp conv ls)));
        val _ = if (res=expected) then () else
                raise ERROR "Compactor: maps over list failure 2";
        (*Setup the initial dynamic compactor...*)
        val comp = Compactor.new_dynamic (Time.fromMilliseconds 10);
        (*Get all of the combinations*)
        val comps = dynamic_combs comp;
        (*Perform all of the tests*)
        fun test_all [] _ = ()
          | test_all (c::cs) n = (
            let
              val res = flat (List.map (fn f => (f())) (
                  Seq.list_of (Compactor.compact_maps_list c conv ls)));
              val _ = if (res=expected) then () else
                      raise ERROR ("Compactor: maps over list failure 3." ^ Int.toString(n));
            in
              ()
            end;test_all cs (n+1));
        val _ = test_all comps 1
      in
        ()
      end) ()
  
    (*Test the compactor when performing maps over a sequence*)
    val _ = Testing.test "Compactor: testing compactor when mapping (maps) over a sequence" (fn () =>
      let
        (*The function to be mapped over the list*)
        fun conv 0 = Seq.of_list (["a","c","d"])
          | conv _ = Seq.of_list (["b"]);
        val rep1 = 17;
        val rep2 = 9;
        (*The list of elements to map over...*)
        val sq = Seq.of_list ((repeat_elem rep1 0)@(repeat_elem rep2 1));
        (*Create the static compactor...*)
        val comp = Compactor.new_static 5;
        val res = flat (List.map (fn f => Seq.list_of (f())) (
                  Seq.list_of (Compactor.compact_maps_seq comp conv sq)));
        val expected = (flat ((repeat_elem rep1 ["a","c","d"])@(repeat_elem rep2 ["b"])));
        val _ = if (res=expected) then () else
                raise ERROR "Compactor: maps over sequence failure 1";
        (*Test when overflowing the first group*)
        val comp = Compactor.new_static 100;
        val res = flat (List.map (fn f => Seq.list_of (f())) (
                  Seq.list_of (Compactor.compact_maps_seq comp conv sq)));
        val _ = if (res=expected) then () else
                raise ERROR "Compactor: maps over sequence failure 2";
        (*Setup the initial dynamic compactor...*)
        val comp = Compactor.new_dynamic (Time.fromMilliseconds 10);
        (*Get all of the combinations*)
        val comps = dynamic_combs comp;
        (*Perform all of the tests*)
        fun test_all [] _ = ()
          | test_all (c::cs) n = (
            let
              val res = flat (List.map (fn f => Seq.list_of (f())) (
                  Seq.list_of (Compactor.compact_maps_seq c conv sq)));
              val _ = if (res=expected) then () else
                      raise ERROR ("Compactor: maps over sequence failure 3." ^ Int.toString(n));
            in
              ()
            end;test_all cs (n+1));
        val _ = test_all comps 1
      in
        ()
      end) ()
      
    (*Test the laziness of the compactor*)
    val _ = Testing.test "Compactor: testing laziness" (fn () =>
      let
        (*Construct an infinite sequence...*)
        fun infinite() = Seq.make (fn () => SOME(1,infinite()));
        (*NThe function to apply over the sequence*)
        fun to_apply _ = 2;
        (*My compactor...*)
        val comp = Compactor.new_static 100;
        (*Map the function...*)
        val static_seq = Compactor.compact_map_seq comp to_apply (infinite());
        (*Dynamic version...*)
        val comp = Compactor.new_dynamic (Time.fromMilliseconds 10);
        val dynamic_seq = Compactor.compact_map_seq comp to_apply (infinite());
        (*Perform the test!*)
        val static_first_elem = fst (the (Seq.pull ((fst (the (Seq.pull static_seq))) ())));
        val dynamic_first_elem = fst (the (Seq.pull ((fst (the (Seq.pull dynamic_seq))) ()))); 
        val _ = if (static_first_elem=dynamic_first_elem andalso dynamic_first_elem=2) then () else
                raise ERROR "Compactor failed laziness test";
      in
        ()
      end) ()
      
      
in
  val _ = Testing.assert_no_failed_tests();
  val _ = "UNIT TESTS FOR COMPACTOR PASSED!"
  
  (*************************************************)
  (*******************SPEED TESTS*******************)
  (*************************************************)
  
  (*These are held in an additional structure since they should not be run at
  compile time*)
  
  structure Test_CompactorSpeed =
  struct
    structure Par_Seq = Safe_Parallel_Seq;
    structure Future = Future_Wrapper;
    structure File = SimpleFileIO;
    (*A structure for handling the file used during testing*)
    structure File_Aux =
    struct
      (*The default file to use*)
      val file = Unsynchronized.ref "../../isaplib/Concurrent/test/compactor-results.txt";
      (*Open the file and be ready to append it...*)
      val is_open = Unsynchronized.ref NONE;
      (*Get the file*)
      fun get_file() = case (!is_open) of
          NONE => raise ERROR "cannot get the file"
        | (SOME f) => f
      (*Test to see if the file is currently open*)
      fun is_file_open() = not (is_none (!is_open))
      (*Open and return the file for appending*)
      fun append() = if (is_file_open()) then get_file()
        else let val ans = File.append (!file) in
          (is_open:=SOME ans;ans) end
      (*Return the file for writing. Wipes out everything
      previously in the file*)
      fun write() = if (is_file_open()) then
        (File.close (get_file());let val ans = File.write (!file) in
          (is_open:=SOME ans;ans) end) else
          let val ans = File.write (!file) in (is_open:=SOME ans;ans) end
      (*Close the file*)
      fun close() = if (is_file_open()) then
          (File.close (get_file());is_open:=NONE;())
        else ()
      (*Remove all the contents in the file.
      This leaves the file closed.*)
      fun renew() =
        (
        if (is_file_open()) then close() else ();
        write();
        close()
        )
    end;
    
    structure Help = Test_ParallelSeqSpeed.Help;
    
    (*Tests for the compactor are quite complicated. There are several predifined
    lists within the tests to demonstrate its performance (lists of lengths and
    hardnesses)*)
    structure Tests =
    struct
      local open Help; infix ---; in
        (*Parameters to state which of the parallel and sequential algorithms should run*)
        val both = 0;
        val parallel_only=1;
        val sequential_only=2;
        
        (*This is the parallel algorithm for the compactor!
        
        Args: the function to be mapped across the sequence ('a -> 'b)
              the time to aim for when compacting in microseconds
              the estimated number of functions to reach this time <optional>
              (
              A limit on how long it can prime for. <optional>
              Whether or not it should become static afterwards
              ) <optional>
              the list of elements which the function should be applied to
        *)
        fun parallel_dynamic_process func time est do_prime long_list =
          let
            (*Set the time*)
            val dyn = Compactor.new_dynamic (Time.fromMicroseconds(time));
            (*Set the estimate*)
            val dyn = Compactor.set_estimated_number dyn est;
            (*Decide how it should be primed, if at all*)
            val dyn = case (do_prime) of
                NONE => Compactor.do_not_prime dyn
              | SOME (limit,go_static) => Compactor.prime dyn limit go_static;
          in
            fn () =>
            let
              (*val _ = PolyML.print("Parallelising over list of length: " ^ (Int.toString(List.length long_list)))*)
              val comp_groups = Par_Seq.of_sequential_seq (fd()) (
                  Compactor.compact_map_seq dyn func (Seq.of_list long_list))
            in
            (*Apply the function*)
            Par_Seq.list_of (
              Par_Seq.maps (fn fd => fn f => Par_Seq.of_sequential_seq fd (f())) comp_groups) end
          end
          
        (*Similar to parallel dynamic process, but this works for static compactors*)
        fun parallel_static_process func size long_list =
          let
            (*Set the time*)
            val dyn = Compactor.new_static size;
          in
            fn () =>
            (*Apply the function*)
            Par_Seq.list_of (
              Par_Seq.maps (fn fd => fn f => Par_Seq.of_sequential_seq fd (f())) (
                Par_Seq.of_sequential_seq (fd()) (
                  Compactor.compact_map_seq dyn func (Seq.of_list long_list))))
          end
        
        (*The sequential version*)
        fun sequential_process func long_list = fn () => Seq.list_of (Seq.map func (Seq.of_list long_list))

        (*Testing different times with the given compactor
        A list is supplied specifying:
        
        (
         The time to aim for in the case of a compactor,
         The length of the list the function is to be applied to,
         The function to apply
        )
        
        It will run the compactor against the sequential algorithm
        *)
        fun test_all comp ls who_runs = 
          let
            fun test_all' [] = ()
              | test_all' ((t,len,hardness)::ts) =
              let
                val func = gen_func hardness;
                (*Generate the list*)
                val ls = List.map (fn _ => ()) (1---len);
                (*Time each function*)
                (*val _ = if (not (who_runs=sequential_only)) then PolyML.print("Running parallel")
                        else PolyML.print("Skipping parallel");*)
                val t2 = 
                  if (not (sequential_only=who_runs)) then (time (if (Compactor.is_dynamic comp) then
                  parallel_dynamic_process func t (Compactor.get_estimated_number comp)
                  (Compactor.check_prime comp) ls
                  else
                  parallel_static_process func (Compactor.get_size comp) ls)) else 0;
                (*val _ = if (not (who_runs=parallel_only)) then PolyML.print("Running sequential")
                        else PolyML.print("Skipping sequential");*)
                val t1 = if (not (parallel_only=who_runs)) then (time (sequential_process func ls))
                  else 0;
                (*The file for writing*)
                val file = File_Aux.append();
              in
                (*Write the results in a file
                
                Format:
                time aimed for, list length, parallel, sequential
                *)
                (
                File.writeStr file ("Time_aimed_at: " ^ LargeInt.toString(t) ^
                  " List_length: " ^ Int.toString(len) ^
                  " Hardness: " ^ Int.toString(hardness) ^
                  (if (not (sequential_only=who_runs)) then (" Parallel: " ^ LargeInt.toString(t2)) else "") ^
                  (if (not (parallel_only=who_runs)) then (" Sequential: " ^ LargeInt.toString(t1)) else "") ^ "\n");
                test_all' ts)
              end
          in
            (File_Aux.renew();
             test_all' ls;
             File_Aux.close())
          end
        
        (*Dummy compactors*)
        val dynamic_dummy = Compactor.new_dynamic (Time.fromMicroseconds(10000))
        val static_dummy = Compactor.new_static 1
        
        (*A good list for checking how the compactor performs trying to run on lists of varyng hardness
        The time aimed at should be constant, as should the length of the list*)
        fun hardness_list len time = let
            val hardnesses = (gen_list 100 1 (fn x => x + 1))@(gen_list 100 110 (fn x => x + 10));
            val n = List.length hardnesses;
            val times = gen_list n time (fn x => x);
            val lengths = gen_list n len (fn x => x);
          in
            compress (zip times (zip lengths hardnesses))
          end
        
        (*A good list for checking how attempts to reach different times fair...*)
        fun times_list len hardness = compress2 (attach (attach (
        (List.map (fn x => x * 10000) (gen_list 5 2 (fn x => x + 4)))@
        (List.map (fn x => x * 100000) (gen_list 5 2 (fn x => x + 4)))) len) hardness)
        
        (*A list of lengths with the given hardness and time to aim for*)
        fun lengths_list time hardness = let
            val lengths = ((List.map (fn x => x * 100) (gen_list 10 5 (fn x=> x + 5)))@
          (List.map (fn x => x * 1000) (gen_list 9 10 (fn x=> x + 5))));
            val n = List.length lengths;
            val hardnesses = gen_list n hardness (fn x => x);
            val times = gen_list n time (fn x => x)
          in
            compress (zip times (zip lengths hardnesses))
          end
        
        (*A list of randomly hard functions, between the two specified integers*)
        fun random_list low high len = gen_list len (gen_func (random low high)) (fn _ => (gen_func (random low high)))
        
        (*Time test a list of functions*)
        fun random_test comp funcs =
          let
            (*Time each function*)
            (*Parallel*)
            val t2 = time (fn() => (
            (Par_Seq.list_of (
            Par_Seq.map (fn _ => fn f => f()) (
            Par_Seq.of_sequential_seq (fd()) (
            Seq.map (fn (x,_) => x) (
            Compactor.compact_with_list comp funcs ())))));()));
            (*Sequential*)
            val t1 = time (fn () => Seq.list_of (
            Seq.map (fn f => f()) (Seq.of_list funcs)));
          in
            TextIO.print(
            "\nParallel took: " ^ LargeInt.toString(t2) ^ "\n" ^
            "Sequential took: " ^ LargeInt.toString(t1) ^ "\n" ^
            "Seq - Par = " ^ LargeInt.toString(t1-t2) ^ "\n" ^
            "Parallel was " ^ Real.toString(Real.fromLargeInt(t1)/Real.fromLargeInt(t2)) ^ " faster than sequential.\n")
          end
  
        (*Test a single run with the compactor as fully dynamic:*)
        fun test_target time len hardness who_runs = test_all dynamic_dummy [(time,len,hardness)] who_runs
        
        (*Test a target set to prime for some limit first*)
        fun test_target_prime time len hardness limit go_static who_runs = test_all
          (Compactor.prime dynamic_dummy limit go_static)
          [(time,len,hardness)] who_runs
        
        (*Test the target times*)
        fun target_time_test len hardness who_runs = test_all dynamic_dummy (times_list len hardness) who_runs
        
        (*Test the length of the list*)
        fun length_time_test time hardness who_runs = test_all dynamic_dummy (lengths_list time hardness) who_runs
        
        (*REMOVE SECONDARY TEST*)
        fun run() = test_all (Compactor.prime dynamic_dummy (SOME 15) false) [(10000,100000,900),(10000,100000,1000),(10000,100000,2000),(10000,100000,3000),(10000,100000,4000),(10000,100000,5000),(10000,100000,6000),(10000,100000,7000),(10000,100000,8000),(10000,100000,9000),(10000,100000,10000)] both;
        
        (*Test the hardness of the functions to be compacted*)
        fun hardness_time_test len time who_runs =
          let
            val comp = dynamic_dummy;
            val comp = Compactor.prime comp (SOME 15) false;
          in
            test_all comp (hardness_list len time) who_runs
          end
        
        (*Test the target times with priming enabled*)
        fun target_time_test_prime len hardness limit go_static who_runs = test_all
          (Compactor.prime dynamic_dummy limit go_static) (times_list len hardness) who_runs
        
        (*Test the target lengths with priming enabled*)
        fun length_test_prime time hardness limit go_static who_runs = test_all
          (Compactor.prime dynamic_dummy limit go_static) (lengths_list time hardness) who_runs
          
        (*Test the random lists with priming enabled*)
        fun random_test_prime low high len t limit go_static =
          random_test (
          Compactor.set_time
          (Compactor.prime dynamic_dummy limit go_static) (Time.fromMicroseconds t))
          (random_list low high len)
          
        (*This test traces the progress of the compactor.*)
        fun trace_test hardnesses comp =
          let
            (*Generate the list of functions...*)
            fun funcs [] = []
              | funcs (h::hs) = (fn () => ([time (gen_func h)]))::(funcs hs);
            val _ = PolyML.print ("Using this many functions: " ^ (Int.toString (List.length (funcs hardnesses))));
            fun f x = x ();
            (*The first problem is to generate groups of tasks via the compactor...*)
            val seq = Seq.of_list (funcs hardnesses);
            (*
            fun compact_with_seq comp seq f_id =
    compact (set_fold_direction comp Left) (fn sq => Seq.pull sq) seq f_id
    *)
            val seq = Compactor.compact_with_seq comp (Seq.map (fn x => fn sq => (f x)::sq) seq) [];
            val par_seq = Par_Seq.of_sequential_seq (fd()) seq;
            val par_seq = Par_Seq.map (fn _ => fn (f,_) => 
                let val sw = Stopwatch.start Stopwatch.new;
                    val size = List.length (f());
                    val t = Time.toMicroseconds(Stopwatch.check sw)
                in (t,size) end) par_seq;
            val _ = PolyML.print ("Running parallel algorithm");
            val res = Seq.list_of (Par_Seq.sequential_seq_of par_seq);
            val _ = PolyML.print ("Writing results");
            val _ = PolyML.print ("Constructed " ^ (Int.toString (List.length res)));
            (*Print out the entire trace...*)
            val file = File_Aux.append();
            fun print_res (t,size) = File.writeStr file ("Group size: " ^ (Int.toString(size)) ^ " Time: " ^ (LargeInt.toString(t)) ^ "\n");
            val _ = map print_res res;
          in
            ()
          end
        
        (*Help to generate traces...*)
        fun make_trace (len_1,h1) (len_2,h2) (len_3,h3) =
          (repeat_elem len_1 h1)@(repeat_elem len_2 h2)@(repeat_elem len_3 h3)
        
        (*Create and run a trace test!*)
        fun create_trace_test time (len_1,h1) (len_2,h2) (len_3,h3) =
          let
            val comp = Compactor.new_dynamic (Time.fromMicroseconds(time));
            val comp = dynamic_dummy;
            val comp = Compactor.prime comp (SOME 10) false;
            (*Make a static compactor!*)
            val comp = Compactor.new_static 100;
          in
            (File_Aux.renew();
            trace_test (make_trace (len_1,h1) (len_2,h2) (len_3,h3)) comp;
            File_Aux.close())
          end
      end;
    end;
  end;
end
