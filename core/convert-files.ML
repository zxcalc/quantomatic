PolyML.SaveState.loadState "heaps/quanto.polyml-heap"; 

datatype filetype = Graph | Ruleset;

fun print_version () =
    (
    TextIO.print "Quantomatic Graph Converter 0.1\n";
    OS.Process.exit OS.Process.success
    )
fun print_help () =
    (
    TextIO.print "Usage: qconvert --theory thname [--graph|--ruleset] in out\n";
    TextIO.print "\n";
    TextIO.print "  -v, --version     Print the version of the quantomatic core and exit\n";
    TextIO.print "  -h, --help        Print this help and exit\n";
    OS.Process.exit OS.Process.success
    )

fun parse_args [] args = args
  | parse_args ("--version"::_) _ = print_version()
  | parse_args ("-v"::_) _ = print_version()
  | parse_args ("--help"::_) _ = print_help()
  | parse_args ("-h"::_) _ = print_help()
  | parse_args ("--theory"::theoryname::ss) (_,ft,l,s) =
      parse_args ss
      (ControllerRegistry.get_eval_func ControllerRegistry.default theoryname,
       ft,l,s)
  | parse_args ("--graph"::ss) (eval_f,_,l,s) =
      parse_args ss
      (eval_f,Graph,l,s)
  | parse_args ("--ruleset"::ss) (eval_f,_,l,s) =
      parse_args ss
      (eval_f,Ruleset,l,s)
  | parse_args (l::ss) (eval_f,ft,"",s) =
      parse_args ss
      (eval_f,ft,l,s)
  | parse_args (s::ss) (eval_f,ft,l,_) =
      parse_args ss
      (eval_f,ft,l,s);

fun do_convert () =
let
  val (eval_func,filetype,load_fn,save_fn) =
    parse_args (CommandLine.arguments()) (NONE,Graph,"","");
  val eval = case eval_func
               of SOME f => f
                | NONE => (TextIO.print "No valid theory given\n";
                           TextIO.print ("Try one of:\n  "^
                            (String.concatWith "\n  "
                            (ControllerRegistry.get_theories
                            ControllerRegistry.default))^"\n");
                           OS.Process.exit OS.Process.failure)
  val _ = if load_fn = ""
          then (TextIO.print "No source filename given\n";
                OS.Process.exit OS.Process.failure)
          else ()
  val _ = if save_fn = ""
          then (TextIO.print "No destination filename given\n";
                OS.Process.exit OS.Process.failure)
          else ()
in
  case filetype
    of Graph =>
        let
          val load_graph_req = Commands.LoadGraphRequest {
                                 details = Commands.LoadGraphFromFileRequest {
                                   fileName = load_fn
                                 }
                               }
          val gn = case eval load_graph_req
                     of Commands.StringResponse { data } => data
                      | Commands.ErrorResponse { message, ... } =>
                          (TextIO.print (message^"\n");
                           OS.Process.exit OS.Process.failure)
                      | _ => raise ERROR "Unexpected response";
          val save_graph_req = Commands.SaveGraphToFileRequest {
                                 graphName = gn,
                                 fileName = save_fn
                               }
          val _ = case eval save_graph_req
                    of Commands.OkResponse => ()
                     | Commands.ErrorResponse { message, ... } =>
                         (TextIO.print (message^"\n");
                          OS.Process.exit OS.Process.failure)
                      | _ => raise ERROR "Unexpected response";
        in
          OS.Process.exit OS.Process.success
        end
     | Ruleset =>
        let
          val load_req = Commands.ImportRulesetFromFileRequest {
                           fileName = load_fn,
                           replace = true
                         }
          val _ = case eval load_req
                    of Commands.OkResponse => ()
                     | Commands.ErrorResponse { message, ... } =>
                         (TextIO.print (message^"\n");
                          OS.Process.exit OS.Process.failure)
                      | _ => raise ERROR "Unexpected response";
          val save_req = Commands.ExportRulesetToFileRequest {
                           fileName = save_fn
                         }
          val _ = case eval save_req
                    of Commands.OkResponse => ()
                     | Commands.ErrorResponse { message, ... } =>
                         (TextIO.print (message^"\n");
                          OS.Process.exit OS.Process.failure)
                      | _ => raise ERROR "Unexpected response";
        in
          OS.Process.exit OS.Process.success
        end
end;

(* forget_all();  *)
PolyML.shareCommonData do_convert; 
PolyML.fullGC ();
PolyML.export ("bin/qconvert", (fn () => do_and_exit_or_die do_convert));
val _ = OS.Process.exit OS.Process.success;
