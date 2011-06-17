use "ROOT.ML";

structure G = GHZW_Theory.Graph;

fun dot tfilenum graph = WriteString.write
  ("/tmp/dot/"^(Int.toString tfilenum)^".dot")
  (GHZW_OutputGraphDot.output graph)


fun output_res res = Tensor.Tab.fold
  (fn (tensor, class) => fn i => (
    WriteString.write ("output/"^(Int.toString i)^".tensor") (Tensor.to_string tensor);
    map_index (fn (j,gr) => (
      WriteString.write ("output/congruence_"^(Int.toString i)^"_"^(Int.toString j)^".dot")
                        (GHZW_OutputGraphDot.output gr)
    )) (EqClass.get_congs class);
    map_index (fn (j,gr) => (
      WriteString.write ("output/redex_"^(Int.toString i)^"_"^(Int.toString j)^".dot")
                        (GHZW_OutputGraphDot.output gr)
    )) (EqClass.get_redexes class);
  i+1)) res 0

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
  val _ = WriteString.write "output/details.txt" details
  val _ = output_res result
  val ret = OS.Process.system "./postprocess.rb"
in (PolyML.print details; synth_result)
end

fun dot_to_svg dot = let
  val (ins,outs) = Unix.streamsOf (Unix.execute ("/usr/bin/env",["dot", "-Tsvg"]))
  val () = TextIO.output (outs, dot)
  (* eat the <?xml.. tag and DOCTYPE *)
  val _ = (TextIO.inputLine ins; TextIO.inputLine ins; TextIO.inputLine ins)
  fun read_all () = case TextIO.inputLine ins
                      of SOME "</svg>\n" => "</svg>\n"
                       | SOME ln => ln ^ (read_all ())
                       | NONE => ""
in read_all ()
end



    
  
      








