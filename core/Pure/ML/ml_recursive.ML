(*  Title:      Pure/ML/ml_recursive.ML
    Author:     Makarius

ML name space for recursive compiler invocation.
*)

signature ML_RECURSIVE =
sig
  type env =
    {debug: bool,
     name_space: PolyML.NameSpace.nameSpace,
     add_breakpoints: (int * (bool ref * Thread_Position.T)) list -> unit};
  val get: unit -> env option
  val recursive: env -> (unit -> 'a) -> 'a
end;

structure ML_Recursive: ML_RECURSIVE =
struct

type env =
  {debug: bool,
   name_space: PolyML.NameSpace.nameSpace,
   add_breakpoints: (int * (bool ref * Thread_Position.T)) list -> unit};

val var = Thread_Data.var () : env Thread_Data.var;

fun get () = Thread_Data.get var;
fun recursive space e = Thread_Data.setmp var (SOME space) e ();

end;
