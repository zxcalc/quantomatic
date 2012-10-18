(* Red/Green *)
local
open RG_Theory
structure G = Graph
structure GIso = BangGraphIso(G)
in
val _ = Testing.test "ruleset rewriting 1: ladder with red spider (all)" (fn () => let
  val ruleset = "rulesets/basic_v2.rules" 
                |> XMLReader.read_from_file
                |> IO_Xml.Input.Ruleset.input;

  val graph = "graphs/d2-ladder_rw1.graph"
              |> XMLReader.read_from_file
              |> IO_Xml.Input.Graph.input;

  val _ = RulesetRewriter.apply_first ruleset graph

  in writeln "success" end) ();

val _ = Testing.test "ruleset rewriting 2: ladder with red spider (full subgraph)" (fn () => let
  val ruleset = "rulesets/basic_v2.rules" 
                |> XMLReader.read_from_file
                |> IO_Xml.Input.Ruleset.input;

  val graph = "graphs/d2-ladder_rw1.graph"
              |> XMLReader.read_from_file
              |> IO_Xml.Input.Graph.input;

  val vset = G.get_vnames graph;
  val _ = RulesetRewriter.apply_first_in ruleset graph vset

  in writeln "success" end) ();
val _ = Testing.test "ruleset rewriting 2: ladder with red spider (rpt)" (fn () => let
  val ruleset = "rulesets/basic_v2_all.rules" 
                |> XMLReader.read_from_file
                |> IO_Xml.Input.Ruleset.input;

  val graph = "graphs/d2-ladder_rw3.graph"
              |> XMLReader.read_from_file
              |> IO_Xml.Input.Graph.input;
  val exp_graph = "graphs/d2-ladder_rw3_norm.graph"
                  |> XMLReader.read_from_file
                  |> IO_Xml.Input.Graph.input;

  fun do_rw g =
    case RulesetRewriter.apply_first ruleset g
      of NONE => g
       | SOME (_,g') => do_rw g'

  val new_graph = do_rw graph
  val _ = if GIso.test new_graph exp_graph then ()
          else (writeln "Expected"; G.print exp_graph;
                writeln "Got"; G.print new_graph;
                raise ERROR "Rewriting gave wrong graph")

  in writeln "success" end) ();
end

(* GHZ/W *)
local
open GHZW_Theory
(*structure Controller = GHZW_Controller*)
structure G = Graph
structure GIso = BangGraphIso(G)
in
val _ = Testing.test "GHZ/W ruleset rewriting 1: 2x2" (fn () => let
  val ruleset = "rulesets/ghz_w/default.rules"
                |> XMLReader.read_from_file
                |> IO_Xml.Input.Ruleset.input;

  val graph = "graphs/ghz_w/2 times 2.graph"
              |> XMLReader.read_from_file
              |> IO_Xml.Input.Graph.input;
  val exp_graph = "graphs/ghz_w/4.graph"
                  |> XMLReader.read_from_file
                  |> IO_Xml.Input.Graph.input;

  fun do_rw g = let
    val vset = G.get_vnames g
    val rwseq = RulesetRewriter.apply_in ruleset g vset
    val rwlist = Seq.list_of rwseq
  in
    case rwlist
      of [] => g
       | ((_,g')::_) => do_rw g'
  end

  val new_graph = do_rw graph
  val _ = if GIso.test new_graph exp_graph then ()
          else (writeln "Expected"; G.print exp_graph;
                writeln "Got"; G.print new_graph;
                raise ERROR "Rewriting gave wrong graph")

  in writeln "success" end) ();

(*
val _ = Testing.test "GHZ/W ruleset rewriting 1: 2x2 (controller)" (fn () => let
  open Commands;
  val OkResponse = Controller.serviceRequest
    (ImportRulesetFromFileRequest {
      fileName = "rulesets/ghz_w/default.rules",
      replace = true
    })
  val StringResponse { data } = Controller.serviceRequest
    (LoadGraphRequest {
      details = LoadGraphFromFileRequest {
                 fileName = "graphs/ghz_w/2 times 2.graph"
               }
    })
  val graph_name = data
  val vertex_names = ["Vk","Vh","Vg","Vw","Vc","Vj","Va","Vb"]
  val CountResponse { count } = Controller.serviceRequest
    (AttachRewritesRequest {
       graphName = graph_name,
       vertexNames = vertex_names
    })

  in writeln "success" end) ();

val _ = Testing.test "GHZ/W ruleset rewriting 2: spider" (fn () => let
  open Commands;
  val OkResponse = Controller.serviceRequest
    (ImportRulesetFromFileRequest {
      fileName = "rulesets/ghz_w/default.rules",
      replace = true
    })
  val StringResponse { data } = Controller.serviceRequest
    (LoadGraphRequest {
      details = LoadGraphFromFileRequest {
                 fileName = "graphs/ghz_w/ghz-spider-simple.graph"
               }
    })
  val graph_name = data
  val vertex_names = ["Va","Vb"]
  val CountResponse { count } = Controller.serviceRequest
    (AttachRewritesRequest {
       graphName = graph_name,
       vertexNames = vertex_names
    })
  val _ = Testing.assert "Count is 1" (count = 1)

  in writeln "success" end) ();
*)
end


val _ = Testing.assert_no_failed_tests();
