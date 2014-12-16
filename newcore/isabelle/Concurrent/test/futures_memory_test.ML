(*This file is designed to see how the Future's library performs when the amount of memory
consumed by threads increases dramatically - in a similar fashion to the parallel tree.

ATTOW, the parallel tree appears to have its threads put to sleep periodically which severely damages
its performance on all the cores. The only possible reason that I know of right now is that this is
connected to high memory usage, but certainly not the (standard) Garbage Collector. Theoretical cases
seem to perform well, and threads show that they are being put to sleep fairly randomly during their
execution.

Since the parallel tree is a rather high level structure and difficult to analyse, this file
aims to reproduce the problem more clearly with bare future calls.

After implementation:
The same problem does appear to exist. Given long enough to run, threads appear to be paused for as long as 12 seconds,
although usually approximately 3 seconds. I have put a few comments in Poly's gc.cpp and it does not appear to be the effect
of a Garbage Collector. Note that the times reported by threads are themselves in the seconds, so it is not a sequential part of the test
that is slowing them down (at least as far as I can see).
*)
structure Test_FuturesMemory =
struct
  (*Some helper functions*)
  
  (*Generate a list of n items given a function and a starting element*)
  fun gen_list 0 _ _ = []
    | gen_list n x f = x::(gen_list (n-1) (f x) f)
    
  (*This function is time consuming. How long it takes can be adjusted
  by the base and factor. Fairly arbitrarily defined - does not attempt to use shared memory!*)
  val base = 5000 div 160
  val factor = 1
  fun expensive () =
    let
      fun runLots 0 _ _ = ()
        | runLots n x t =
          if (x>=(base div 10) * factor) then runLots (n-1) 1 (t+1)
          else runLots (n-1) (x+1) (t+1)
    in
      runLots (factor * base) 1 1
    end
  
  (*Repeat a function expecting the unit "n" times*)
  fun repeat 0 _ = ()
    | repeat n f = (f();repeat (n-1) f)
  
  (*Creates a function of a certain grade of time consumption*)
  fun gen_func n = fn () => repeat n expensive
  
  (*A fairly large structure
  Since in the case where the parallel tree is used, implicit sharing within PolyML
  is being used across record structures, I will use records here...*)
  datatype BIG = Mem of {
    fill_me : int list (*This list will grow and so use up increasingly more memory*)
  }
  
  (*A default starting Mem...*)
  val start_mem = Mem {
    fill_me = []
  }
  
  (*Identity function*)
  fun id x = x;
  
  (*THE IDEA
  
  There will be an external thread assigning many tasks to the futures library in groups.
  These will do some pointless work, and then return a list of results based on the structure above.
  This result will be saved in a list to be returned at the end.
  Each element in the given list is then processed in parallel, much like the parallel tree.
  This continues until a certain depth is reached.
  
  The functions, during their pointless work, will add a large mount to the fill_me section each time they run.
  *)
  
  fun task hardness (Mem m) =
    let
      val t1 = Time.now(); (*To detect when tasks take an unusually large amount of time*)
      val _ = gen_func hardness (); (*Use up some time*)
      val current_list = #fill_me m;
      val to_add_to_list = (*Create new elements to be added to the list*)
          gen_list 100 0 id
      val new_m = Mem {
        fill_me = to_add_to_list @ current_list
      }
      val t2 = Time.toMicroseconds ((Time.now()) - t1);
      (*For hardness 10000, 500000us is a lot longer than you would expect*)
      val _ = if (t2>500000) then (PolyML.print("Required this amount of time: " ^ (LargeInt.toString(t2)));())
              else ();
    in (*Should be memory intensive!*)
      gen_list 2 new_m id
    end
    
  (*Reverse a list quickly...*)
  fun reverse xs = let
    fun revcat [] ys = ys
      | revcat (x::xs) ys = revcat xs (x::ys)
    in
      revcat xs []
    end
    
  (*Now a function which will apply the futures library over all
  of its elements, and return all of their results as a new list...*)
  fun apply_futures _ [] [] = []
      (*Joining phase*)
    | apply_futures hardness [] (y::ys) = (Future.join y) @ (apply_futures hardness [] ys)
      (*About to join. Join in order (i.e: FIFO)*)
    | apply_futures hardness [x] ys = apply_futures hardness [] (
      let
        val _ = PolyML.print("Reversing list");
        val res = reverse ((Future.fork (fn () => task hardness x))::ys);
        val _ = PolyML.print("Done reversing");
      in
        res
      end)
      (*Spawning phase*)
    | apply_futures hardness (x::xs) ys = apply_futures hardness xs ((Future.fork (fn () => task hardness x))::ys)
  
  (*Now recursively perform the above process on all elements some number of times
  (It is not particularly my concern that this terminates within any reasonable
  amount of time...)*)
  fun apply_rec _ 0 ms = ms
    | apply_rec hardness n ms = apply_rec hardness (n-1) (apply_futures hardness ms [])
    
  (*Run the test at the specificed level of hardness and recursive depth.
  Initial is how many Mem records to begin with*)
  fun run_test hardness depth initial =
    apply_rec hardness depth (gen_list initial start_mem id)
  
  (*Recommended test settings to display the problem*)
  fun run() = run_test 10000 100 50
end;
