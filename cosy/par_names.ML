signature PAR_NAMES =
sig
  structure Name : NAMES
  exception FOUND
  exception ERROR
  val process_nset : (Name.NSet.T -> 'a) -> Name.NSet.T -> ('a Exn.result) list
end

functor Par_Name (
  structure Name : NAMES
) : PAR_NAMES =
struct
  structure Name = Name
  
  (* general-purpose exceptions used to signal the top-level join and cancel other futures *)
  exception FOUND
  exception ERROR
  
  fun process_nset f nset = let
    val nsets = Name.NSet.subdivide (Thread.numProcessors ()) nset
    val group = Task_Queue.new_group (Future.worker_group ())
    (*val cancelf = fn () => Future.cancel_group group*)
    val futures = Future.forks {name = "process_nset", group = SOME group, deps = [], pri = 0}
                  (map (fn ns => fn () => f ns) nsets)
  in Future.join_results futures
  end
  
end

structure Par_RuleName = Par_Name(structure Name = RuleName)
