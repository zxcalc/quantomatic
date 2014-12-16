val print = PolyML.print;

(* a bit hacky here, maybe move to library.ML or basics.ML *)
fun maybef f NONE = NONE
  | maybef f (SOME x) = SOME (f x);
fun then_try f1 f2 x = (case f1 x of NONE => NONE | SOME x2 => f2 x2);

fun while_some f a = 
  case f a of NONE => a
            | SOME a' => while_some f a';

exception tryify_failed of unit
      
fun tryify ff f x = 
    (SOME (ff (fn x => case f x of NONE => raise tryify_failed () 
                                 | SOME y => y)
              x))
    handle tryify_failed () => NONE;

fun untryify f x =  
    (case f x of NONE => raise tryify_failed ()
     | SOME y => y);
         
     
fun do_and_exit_or_die f = 
    (PolyML.exception_trace f; OS.Process.exit OS.Process.success : unit)
    handle _ => OS.Process.exit OS.Process.failure;
fun forget_all () = 
    (map PolyML.Compiler.forgetSignature
         (PolyML.Compiler.signatureNames());
     map PolyML.Compiler.forgetType
         (PolyML.Compiler.typeNames());
     map PolyML.Compiler.forgetValue
         (List.filter (fn s => s <> "run" andalso s <> "do_and_exit_or_die"
                          andalso s <> "forget_all")
                 (PolyML.Compiler.valueNames()));
     map PolyML.Compiler.forgetStructure
         (List.filter (fn s => s <> "PolyML" andalso s <> "OS") 
                 (PolyML.Compiler.structureNames()));
     ());
