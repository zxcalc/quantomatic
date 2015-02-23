PolyML.Project.make "quanto-xml-to-json/xml_parse_utils.ML";
PolyML.Project.make "quanto-xml-to-json/xml_input.ML";
PolyML.Project.make "quanto-xml-to-json/linrat_xml.ML";
PolyML.Project.make "quanto-xml-to-json/graph_xml_v1_input.ML";
PolyML.Project.make "quanto-xml-to-json/graph_xml_v2_input.ML";
PolyML.Project.make "quanto-xml-to-json/graph_annotations_xml_input.ML";
PolyML.Project.make "quanto-xml-to-json/rule_xml_input.ML";
PolyML.Project.make "quanto-xml-to-json/ruleset_annotations_xml_input.ML";
PolyML.Project.make "quanto-xml-to-json/ruleset_xml_input.ML";
PolyML.Project.make "quanto-xml-to-json/convert_theory.ML";

val converters = [
  (GHZW_Theory.theory_name, (GHZW_Converter.convert_graph,
                             GHZW_Converter.convert_rule,
                             GHZW_Converter.convert_ruleset)),
  (RTechn_Theory.theory_name, (RTechn_Converter.convert_graph,
                               RTechn_Converter.convert_rule,
                               RTechn_Converter.convert_ruleset)),
  (Petri_Theory.theory_name, (Petri_Converter.convert_graph,
                              Petri_Converter.convert_rule,
                              Petri_Converter.convert_ruleset)),
  (RG_Theory.theory_name, (RG_Converter.convert_graph,
                           RG_Converter.convert_rule,
                           RG_Converter.convert_ruleset)),
  (RGB_Theory.theory_name, (RGB_Converter.convert_graph,
                            RGB_Converter.convert_rule,
                            RGB_Converter.convert_ruleset)),
  (Substrings_Theory.theory_name, (Substrings_Converter.convert_graph,
                                   Substrings_Converter.convert_rule,
                                   Substrings_Converter.convert_ruleset)),
  (Substr_Linrat_Theory.theory_name, (Substr_Linrat_Converter.convert_graph,
                                      Substr_Linrat_Converter.convert_rule,
                                      Substr_Linrat_Converter.convert_ruleset))
]

fun read_xml_file file = XML.parse (File_Io.read_string file)
  handle Library.ERROR msg =>
           (TextIO.print "File is not valid XML: ";
            TextIO.print msg;
            TextIO.print "\n";
            OS.Process.exit OS.Process.failure)
       | File_Io.io_exn msg =>
           (TextIO.print msg;
            TextIO.print "\n";
            OS.Process.exit OS.Process.failure)
fun write_json_file file s = File_Io.write_json file s
  handle File_Io.io_exn msg =>
           (TextIO.print msg;
            TextIO.print "\n";
            OS.Process.exit OS.Process.failure)

fun print_version () =
    (
    TextIO.print "Quantomatic Graph Converter 1.0\n";
    OS.Process.exit OS.Process.success
    )
fun print_help () =
    (
    TextIO.print ("Usage: "^(CommandLine.name())^" [--graph|--rule|--ruleset] theory_name infile outfile\n");
    TextIO.print "Converts old XML-style Quantomatic graphs and rulesets to the new JSON format\n";
    TextIO.print "\n";
    TextIO.print "  --graph         Input file is a graph\n";
    TextIO.print "  --rule          Input file is a rule\n";
    TextIO.print "  --ruleset       Input file is a ruleset\n";
    TextIO.print "  -v, --version   Print the version of the quantomatic core and exit\n";
    TextIO.print "  -h, --help      Print this help and exit\n";
    OS.Process.exit OS.Process.success
    )

datatype filetype = Graph | Rule | Ruleset | Auto
type args = {
  theory : string,
  filetype : filetype,
  infile : string,
  outfile : string
}

fun parse_args [] args =
      if #theory args = ""
      then (TextIO.print "No theory given\n";
            print_help();
            OS.Process.exit OS.Process.failure)
      else if #infile args = ""
      then (TextIO.print "No source filename given\n";
            print_help();
            OS.Process.exit OS.Process.failure)
      else if #outfile args = ""
      then (TextIO.print "No destination filename given\n";
            print_help();
            OS.Process.exit OS.Process.failure)
      else args
  | parse_args ("--version"::_) _ = print_version()
  | parse_args ("-v"::_) _ = print_version()
  | parse_args ("--help"::_) _ = print_help()
  | parse_args ("-h"::_) _ = print_help()
  | parse_args ("--graph"::ss) args =
      parse_args ss {
        theory = #theory args,
        filetype = Graph,
        infile = #infile args,
        outfile = #outfile args
      }
  | parse_args ("--rule"::ss) args =
      parse_args ss {
        theory = #theory args,
        filetype = Rule,
        infile = #infile args,
        outfile = #outfile args
      }
  | parse_args ("--ruleset"::ss) args =
      parse_args ss {
        theory = #theory args,
        filetype = Ruleset,
        infile = #infile args,
        outfile = #outfile args
      }
  | parse_args (theory::ss) (args as {theory = "",...}) =
      parse_args ss {
        theory = theory,
        filetype = #filetype args,
        infile = #infile args,
        outfile = #outfile args
      }
  | parse_args (infile::ss) (args as {infile = "",...}) =
      parse_args ss {
        theory = #theory args,
        filetype = #filetype args,
        infile = infile,
        outfile = #outfile args
      }
  | parse_args (outfile::ss) (args as {outfile = "",...}) =
      parse_args ss {
        theory = #theory args,
        filetype = #filetype args,
        infile = #infile args,
        outfile = outfile
      }
  | parse_args _ _ =
      (TextIO.print "Too many arguments\n"; print_help())


fun main () =
let
  val args =
    parse_args (CommandLine.arguments()) {
      theory = "", filetype = Auto, infile = "", outfile = ""
    }
  val filetype =
    if #filetype args <> Auto then #filetype args
    else if String.isSuffix ".graph" (#infile args)
    then Graph
    else if String.isSuffix ".rule" (#infile args)
    then Rule
    else if String.isSuffix ".rules" (#infile args)
    then Ruleset
    else if String.isSuffix ".ruleset" (#infile args)
    then Ruleset
    else (TextIO.print "Cannot guess file type; please specify --graph or --ruleset";
          OS.Process.exit OS.Process.failure)
  val convert =
    case List.find (fn (th,_) => th = #theory args) converters
      of SOME (_,(g_f,r_f,rs_f)) =>
            (case filetype
               of Graph => g_f
                | Rule => r_f
                | Ruleset => rs_f
                | Auto => raise ERROR "should not get here")
       | NONE =>
           (TextIO.print "Unknown theory \"";
            TextIO.print (#theory args);
            TextIO.print "\". Available theories:\n";
            fold (fn (th,_) => K (TextIO.print ("  "^th^"\n"))) converters ();
            OS.Process.exit OS.Process.failure)
in
  read_xml_file (#infile args)
       |> convert
       |> write_json_file (#outfile args)
end
  handle XML_Parse_Utils.bad_input_exp (msg,loc) =>
    (TextIO.print "Invalid input file: ";
     TextIO.print msg;
     TextIO.print " at ";
     TextIO.print loc;
     TextIO.print "\n";
     OS.Process.exit OS.Process.failure)

(* vi:ts=2:sw=2:sts=2:et
*)
