(*  Title:      Pure/ML/ml_statistics.ML
    Author:     Makarius

ML runtime statistics.
*)

signature ML_STATISTICS =
sig
  val get: unit -> Properties.T
end;

structure ML_Statistics: ML_STATISTICS =
struct

fun get () =
  let
    val
     {gcFullGCs,
      gcPartialGCs,
      sizeAllocation,
      sizeAllocationFree,
      sizeHeap,
      sizeHeapFreeLastFullGC,
      sizeHeapFreeLastGC,
      threadsInML,
      threadsTotal,
      threadsWaitCondVar,
      threadsWaitIO,
      threadsWaitMutex,
      threadsWaitSignal,
      timeGCSystem,
      timeGCUser,
      timeNonGCSystem,
      timeNonGCUser,
      userCounters} = PolyML.Statistics.getLocalStats ();
    val user_counters =
      Vector.foldri
        (fn (i, j, res) => ("user_counter" ^ Value.print_int i, Value.print_int j) :: res)
        [] userCounters;
  in
    [("full_GCs", Value.print_int gcFullGCs),
     ("partial_GCs", Value.print_int gcPartialGCs),
     ("size_allocation", Value.print_int sizeAllocation),
     ("size_allocation_free", Value.print_int sizeAllocationFree),
     ("size_heap", Value.print_int sizeHeap),
     ("size_heap_free_last_full_GC", Value.print_int sizeHeapFreeLastFullGC),
     ("size_heap_free_last_GC", Value.print_int sizeHeapFreeLastGC),
     ("threads_in_ML", Value.print_int threadsInML),
     ("threads_total", Value.print_int threadsTotal),
     ("threads_wait_condvar", Value.print_int threadsWaitCondVar),
     ("threads_wait_IO", Value.print_int threadsWaitIO),
     ("threads_wait_mutex", Value.print_int threadsWaitMutex),
     ("threads_wait_signal", Value.print_int threadsWaitSignal),
     ("time_CPU", Value.print_real (Time.toReal timeNonGCSystem + Time.toReal timeNonGCUser)),
     ("time_GC", Value.print_real (Time.toReal timeGCSystem + Time.toReal timeGCUser))] @
    user_counters
  end;

end;
