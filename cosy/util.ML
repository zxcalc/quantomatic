PolyML.SaveState.loadState "../core/heaps/quanto.heap";
use "ROOT.ML";


fun write_file file str = let
  val outs = TextIO.openOut file
  val _ = TextIO.outputSubstr (outs, Substring.full (str)) 
  val _ = TextIO.closeOut outs
in ()
end

(*structure GHZW_TensorData : TENSOR_DATA = 
struct
  structure Graph = GHZW_Theory.Graph
  structure Tensor = IntTensor
  
  val dimension = 2
  
  fun ghz (maxi, maxj) (i,j) = if ((i=0 andalso j=0) orelse (i=maxi andalso j=maxj)) then 1 else 0
  fun w (ins,outs) (i,j) = let
    val outsum = List.foldr (op+) 0 (Tensor.decompose_index 2 outs i)
    val insum = List.foldr (op+) 0 (Tensor.decompose_index 2 ins j)
  in if ((outsum = 1 andalso insum = ins) orelse (outsum = 0 andalso insum = (ins - 1))) then 1 else 0
  end
  

  fun tensor_for_vertex_data (Graph.NVert GHZW_Data.GHZ) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (ghz ((Tensor.pow 2 outs)-1, (Tensor.pow 2 ins)-1))
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.W) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (w (ins,outs))
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.TICK) (1,1) =
        Tensor.tensor (2,1,1) [0,1,1,0]
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.TICK) (ins,outs) =
        Tensor.tensorf (2,ins,outs) (K 0)  (* just return a zero tensor if bad dimensions on tick *)
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.ZERO) (ins,outs) =
        Tensor.tensorf (2,ins,outs) (K 0)  (* always return 0 tensor *)
    | tensor_for_vertex_data (Graph.WVert) _ = Tensor.id dimension 1
end

structure GHZW_Enum = GraphEnumerator(structure Theory = GHZW_Theory)

structure GHZW_Metric = WeightedArityMetric(
  structure Graph = GHZW_Theory.Graph
  fun weight_for_data (Graph.NVert GHZW_Data.ZERO) = 1
    | weight_for_data _ = 5
)

structure GHZW_DefaultSynth = TensorSynth(
  structure GraphEnum = GHZW_Enum
  structure Metric = GHZW_Metric
  structure TData = GHZW_TensorData
)

structure GHZW_RSBuilder = RulesetBuilder(structure Synthesize = GHZW_DefaultSynth)
structure GHZW_Gens = DefaultGenerators(structure Graph = GHZW_Theory.Graph)
structure GHZW_Rws = DefaultRewrites(structure Rule = GHZW_Theory.Rule)



structure G = GHZW_Theory.Graph;

(*fun dot tfilenum graph = write_file
  ("/tmp/dot/"^(Int.toString tfilenum)^".dot")
  (GHZW_OutputGraphDot.output graph)
*)

fun output_res res = () (*Tensor.Tab.fold
  (fn (tensor, class) => fn i => (
    write_file ("output/"^(Int.toString i)^".tensor") (Tensor.to_string tensor);
    map_index (fn (j,gr) => (
      write_file ("output/congruence_"^(Int.toString i)^"_"^(Int.toString j)^".dot")
                        (GHZW_OutputGraphDot.output gr)
    )) (EqClass.get_congs class);
    map_index (fn (j,gr) => (
      write_file ("output/redex_"^(Int.toString i)^"_"^(Int.toString j)^".dot")
                        (GHZW_OutputGraphDot.output gr)
    )) (EqClass.get_redexes class);
  i+1)) res 0*)

fun synth (run as (ins,outs,verts,plugs)) = let
  val gens = GHZW_Gens.gen_list 3 [GHZW_VertexData.GHZ,GHZW_VertexData.W]
  val synth_result = GHZW_DefaultSynth.synth gens run
  val (num_classes, num_congs, num_redexes) = GHZW_DefaultSynth.stats synth_result
  val details =
    "SYNTHESIS RESULTS\n"^
    "-----------------------------------------\n"^
    "  "^(Int.toString ins)^" inputs\n"^
    "  "^(Int.toString outs)^" outputs\n"^
    "  "^(Int.toString verts)^" max vertices\n"^
    "  "^(Int.toString plugs)^" max pluggings\n"^
    "-----------------------------------------\n"^
    "  Found "^(Int.toString num_classes)^" equivalence classes.\n"^
    "  Average class size: "^(Int.toString ((num_congs + num_redexes) div num_classes))^".\n"^
    "-----------------------------------------\n"
  val _ = OS.Process.system "mkdir -p output; rm -f output/*"
  val _ = write_file "output/details.txt" details
  val _ = output_res result
  (*val ret = OS.Process.system "./postprocess.rb"*)
in (PolyML.print details; synth_result)
end*)

(*fun dot_to_svg dot = let
  val (ins,outs) = Unix.streamsOf (Unix.execute ("/usr/bin/env",["dot", "-Tsvg"]))
  val () = TextIO.output (outs, dot)
  (* eat the <?xml.. tag and DOCTYPE *)
  val _ = (TextIO.inputLine ins; TextIO.inputLine ins; TextIO.inputLine ins)
  fun read_all () = case TextIO.inputLine ins
                      of SOME "</svg>\n" => "</svg>\n"
                       | SOME ln => ln ^ (read_all ())
                       | NONE => ""
in read_all ()
end*)



    
  
      








