(* Red/Green *)
local
  open RG_Theory
  structure G = Graph
  structure HomeoFinder = BangGraphHomeomorphismSearcher(Graph)
in
val _ = Testing.test "ruleset rewriting 1: ladder with red spider (all)" (fn () => let
  val ruleset = "rulesets/red_green/basic.rules"
                |> Json.read_file
                |> RulesetJSON.input

  val graph = "graphs/red_green/d2-ladder_rw1.graph"
              |> Json.read_file
              |> GraphJSON.input

  val _ = RulesetRewriter.apply_first ruleset graph

  in () end) ()

val _ = Testing.test "ruleset rewriting 2: ladder with red spider (full subgraph)" (fn () => let
  val ruleset = "rulesets/red_green/basic.rules"
                |> Json.read_file
                |> RulesetJSON.input

  val graph = "graphs/red_green/d2-ladder_rw1.graph"
              |> Json.read_file
              |> GraphJSON.input

  val vset = G.get_vertices graph
  val _ = RulesetRewriter.apply_first_in ruleset graph vset

  in () end) ()
val _ = Testing.test "ruleset rewriting 2: ladder with red spider (rpt)" (fn () => let
  val ruleset = "rulesets/red_green/basic_all.rules" 
                |> Json.read_file
                |> RulesetJSON.input

  val graph = "graphs/red_green/d2-ladder_rw3.graph"
              |> Json.read_file
              |> GraphJSON.input
  val exp_graph = "graphs/red_green/d2-ladder_rw3_norm.graph"
                  |> Json.read_file
                  |> GraphJSON.input

  fun do_rw g =
    case RulesetRewriter.apply_first ruleset g
      of NONE => g
       | SOME (_,g') => do_rw g'

  val new_graph = do_rw graph
  val _ = if HomeoFinder.is_homeomorphic new_graph exp_graph then ()
          else (writeln "Expected"; G.print exp_graph;
                writeln "Got"; G.print new_graph;
                raise ERROR "Rewriting gave wrong graph")
  in () end) ()


  val bb_graph = "graphs/red_green/bbox.qgraph"
        |> Json.read_file |> GraphJSON.input
  val bb_rule = Rule.mk (bb_graph, bb_graph)

  val bb_matches = MatchSearch.match bb_graph bb_graph
  val _ = Testing.test "red-green !-graph should match itself" (fn () => (
      if length (Seq.list_of bb_matches) >= 1 then ()
      else raise ERROR "No matches found"
    )) ()

  val bb_rewrites = Rewriter.find_rewrites bb_rule bb_graph
  val _ = Testing.test "red-green !-graph rule should match its own LHS" (fn () => (
      if length (Seq.list_of bb_rewrites) >= 1 then ()
      else raise ERROR "No matches found"
    )) ()

  val bb_graph = "graphs/red_green/gen-bialg-inst.qgraph"
        |> Json.read_file |> GraphJSON.input
  val bb_rule = Rule.mk (bb_graph, bb_graph)

  val _ = PolyML.exception_trace (fn () => Seq.list_of bb_matches)

  val bb_matches = MatchSearch.match bb_graph bb_graph
  val _ = Testing.test "gen-bialg !-graph should match itself" (fn () => (
      if length (Seq.list_of bb_matches) >= 1 then ()
      else raise ERROR "No matches found"
    )) ()


  (*val bb_rewrites = Rewriter.find_rewrites bb_rule bb_graph
  val _ = Testing.test "gen-bialg !-graph rule should match its own LHS" (fn () => (
      if length (Seq.list_of bb_rewrites) >= 1 then ()
      else raise ERROR "No matches found"
    )) ()*)

end

(* GHZ/W *)
local
  open GHZW_Theory
  structure Controller = GHZW_Controller
  structure G = Graph
  structure HomeoFinder = BangGraphHomeomorphismSearcher(Graph)
  fun ctrlr_assert_ok Commands.OkResponse = ()
    | ctrlr_assert_ok _ = raise ERROR "Unexpected response from controller"
  fun ctrlr_assert_string (Commands.StringResponse { data }) = data
    | ctrlr_assert_string _ = raise ERROR "Unexpected response from controller"
  fun ctrlr_assert_count (Commands.CountResponse { count }) = count
    | ctrlr_assert_count _ = raise ERROR "Unexpected response from controller"
in
val _ = Testing.test "GHZ/W ruleset rewriting 1: 2x2" (fn () => let
  val ruleset = "rulesets/ghz_w/default.rules"
                |> Json.read_file
                |> RulesetJSON.input

  val graph = "graphs/ghz_w/2 times 2.graph"
              |> Json.read_file
              |> GraphJSON.input
  val exp_graph = "graphs/ghz_w/4.graph"
                  |> Json.read_file
                  |> GraphJSON.input

  fun do_rw g = let
    val vset = G.get_vertices g
    val rwseq = RulesetRewriter.apply_in ruleset g vset
    val rwlist = Seq.list_of rwseq
  in
    case rwlist
      of [] => g
       | ((_,g')::_) => do_rw g'
  end

  val new_graph = do_rw graph
  val _ = if HomeoFinder.is_homeomorphic new_graph exp_graph then ()
          else (writeln "Expected"; G.print exp_graph;
                writeln "Got"; G.print new_graph;
                raise ERROR "Rewriting gave wrong graph")
  in () end) ()

val _ = Testing.test "GHZ/W ruleset rewriting 1: 2x2 (controller)" (fn () => let
  open Commands
  val _ = ctrlr_assert_ok (Controller.serviceRequest
    (ImportRulesetFromFileRequest {
      fileName = "rulesets/ghz_w/default.rules",
      replace = true
    }))
  val data = ctrlr_assert_string (Controller.serviceRequest
    (LoadGraphRequest {
      details = LoadGraphFromFileRequest {
                 fileName = "graphs/ghz_w/2 times 2.graph"
               }
    }))
  val graph_name = data
  val vertex_names = ["Vk","Vh","Vg","Vw","Vc","Vj","Va","Vb"]
  val _ = ctrlr_assert_count (Controller.serviceRequest
    (AttachRewritesRequest {
       graphName = graph_name,
       vertexNames = vertex_names
    }))

  in () end) ()

val _ = Testing.test "GHZ/W ruleset rewriting 2: spider" (fn () => let
  open Commands
  val _ = ctrlr_assert_ok (Controller.serviceRequest
    (ImportRulesetFromFileRequest {
      fileName = "rulesets/ghz_w/default.rules",
      replace = true
    }))
  val data = ctrlr_assert_string (Controller.serviceRequest
    (LoadGraphRequest {
      details = LoadGraphFromFileRequest {
                 fileName = "graphs/ghz_w/ghz-spider-simple.graph"
               }
    }))
  val graph_name = data
  val vertex_names = ["Va","Vb"]
  val count = ctrlr_assert_count (Controller.serviceRequest
    (AttachRewritesRequest {
       graphName = graph_name,
       vertexNames = vertex_names
    }))
  val _ = Testing.assert "Count is 1" (count = 1)

  in () end) ()
end

val _ = Testing.assert_no_failed_tests()
