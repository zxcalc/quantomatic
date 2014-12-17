(*  Title:      Pure/ML/ml_statistics_polyml-5.5.0.ML
    Author:     Makarius

ML runtime statistics for Poly/ML 5.5.0.
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
        (fn (i, j, res) => ("user_counter" ^ Markup.print_int i, Markup.print_int j) :: res)
        [] userCounters;
  in
    [("full_GCs", Markup.print_int gcFullGCs),
     ("partial_GCs", Markup.print_int gcPartialGCs),
     ("size_allocation", Markup.print_int sizeAllocation),
     ("size_allocation_free", Markup.print_int sizeAllocationFree),
     ("size_heap", Markup.print_int sizeHeap),
     ("size_heap_free_last_full_GC", Markup.print_int sizeHeapFreeLastFullGC),
     ("size_heap_free_last_GC", Markup.print_int sizeHeapFreeLastGC),
     ("threads_in_ML", Markup.print_int threadsInML),
     ("threads_total", Markup.print_int threadsTotal),
     ("threads_wait_condvar", Markup.print_int threadsWaitCondVar),
     ("threads_wait_IO", Markup.print_int threadsWaitIO),
     ("threads_wait_mutex", Markup.print_int threadsWaitMutex),
     ("threads_wait_signal", Markup.print_int threadsWaitSignal),
     ("time_CPU", Markup.print_real (Time.toReal timeNonGCSystem + Time.toReal timeNonGCUser)),
     ("time_GC", Markup.print_real (Time.toReal timeGCSystem + Time.toReal timeGCUser))] @
    user_counters
  end;

end;

