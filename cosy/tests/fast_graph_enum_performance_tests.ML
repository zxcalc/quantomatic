functor CosyUtil(
  structure Enum : GRAPH_ENUM
  val data_list : Enum.Theory.OVData.IData.data list
  val output_dot : Enum.Theory.Graph.T -> string
  val initial_rs : Enum.Theory.Ruleset.T
)=
struct

structure Enum = Enum
structure EqClassTab = Enum.EqClassTab
structure EqClass = EqClassTab.EqClass
structure GraphEntry = EqClassTab.GraphEntry
structure Theory = Enum.Theory

val gens = let
  fun gens_for d = [
    (Theory.OVData.NVert d,1,2),
    (Theory.OVData.NVert d,2,1),
    (Theory.OVData.NVert d,2,0),
    (Theory.OVData.NVert d,0,2),
    (Theory.OVData.NVert d,1,0),
    (Theory.OVData.NVert d,0,1)
  ]
in maps gens_for data_list
end

fun run_dot () = Unix.streamsOf (Unix.execute ("/usr/bin/env",["dot", "-Tsvg"]))
fun close_dot (ins,outs) = (TextIO.closeIn ins; TextIO.closeOut outs)

fun output_eqtab eqt (vertices, inputs, outputs, edges) = let
  (*Calculate some general data about the class*)
  val classes = EqClassTab.fold_eqclasses cons eqt []
  val num_classes = List.length classes
  val num_congs = List.length (flat (map EqClass.get_congs classes))
  val num_redexes = List.length (flat (map EqClass.get_redexes classes))
  (*Output some general information*)
  val details =
    "SYNTHESIS RESULTS\n"^
    "-----------------------------------------\n"
        ^
    "  "^(Int.toString vertices)^" max vertices\n"^
    "  "^(Int.toString inputs)^" max inputs\n"^
    "  "^(Int.toString outputs)^" max outputs\n"^
    "  "^(Int.toString edges)^" max edges\n"^
    "-----------------------------------------\n"^
    "  Found "^(Int.toString num_classes)^" equivalence classes.\n"^
    "  Average class size: "^(Int.toString ((num_congs + num_redexes) div num_classes))^".\n"^
    "-----------------------------------------\n"
  val io = run_dot ()
  val _ = PolyML.print(details)
  fun output_class class i = let
    (*Output the class with its representative*)
    val _ = TextIO.print ("Class " ^ (Int.toString i) ^ "\n")
    val rep = EqClassTab.get_graph_entry eqt (EqClass.get_rep class)
    val _ = case GraphEntry.get_edata rep
              of SOME d => TextIO.print ((GraphEntry.Equiv.to_string d))
               | NONE   => TextIO.print (("No Equiv data") ^ "\n")
    (*Print a graph to the screen*)
    fun output_graph len (i, gr) = if i = 100 then TextIO.print ((Int.toString (len - 100) ^ " more...\n"))
                                     else (if i < 100 then (TextIO.print ((output_dot gr))) else TextIO.print "\n")
    val _ = TextIO.print("\n");
    val congruences = map (GraphEntry.get_graph o EqClassTab.get_graph_entry eqt) (EqClass.get_congs class)
    val redexes = map (GraphEntry.get_graph o EqClassTab.get_graph_entry eqt) (EqClass.get_redexes class)
    (*Print the congruences*)
    val _ = TextIO.print ("Has " ^ (Int.toString (List.length congruences)) ^ " Congruences\n")
    val _ = map_index (output_graph (length congruences)) congruences
    (*Print all of the reducible expressions*)
    val _ = TextIO.print ("Has " ^ (Int.toString (List.length redexes)) ^ " Reducible Expressions\n")
    val _ = map_index (output_graph (length redexes)) redexes
  in i+1
  end
  val _ = EqClassTab.fold_eqclasses output_class eqt 1
  val _ = close_dot io
in ()
end

end

val rg_data_list = [RG_InternVData.Xnd LinratAngleExpr.zero,
                    RG_InternVData.Znd LinratAngleExpr.zero]
structure RGCosy = CosyUtil(
  structure Enum = RG_Enum
  val data_list = rg_data_list
  val output_dot = RG_GraphicalTheoryIO.OutputGraphDot.output
  val initial_rs = RG_Spiders.frob_and_special_rules data_list
)

val ghzw_data_list = [GHZW_Data.GHZ, GHZW_Data.W]
structure GHZWCosy = CosyUtil(
  structure Enum = GHZW_Enum
  val data_list = ghzw_data_list
  val output_dot = GHZW_GraphicalTheoryIO.OutputGraphDot.output
  val initial_rs = GHZW_Spiders.frob_rules data_list
)

(*Run the algorithm!!! Please...*)
structure Test_FastGraphEnumSpeed =
struct
  (*max number of: (vertices, inputs, outputs, edges)*)
  val limits = (2,2,2,2)
  (*Run the enumerator for some amount of time...*)
  fun run() = RGCosy.output_eqtab (RGCosy.Enum.tab_enum RGCosy.gens limits) limits;
  
end;
