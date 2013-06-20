signature GRAPH_COMPONENT_DATA_IO =
sig
  type nvdata;
  type edata;

  structure IVDataInputJSON : INPUT_JSON
    where type data = nvdata;
  structure IVDataOutputJSON : OUTPUT_JSON
    where type data = nvdata;
  structure EDataInputJSON : INPUT_JSON
    where type data = edata;
  structure EDataOutputJSON : OUTPUT_JSON
    where type data = edata;
  structure DotStyle : DOT_STYLE
    where type nvdata = nvdata;
end;

functor StringTableAnnotatedGraphicalTheoryIO (
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = GraphComponentDataIO.nvdata
  sharing type Theory.Graph.edata = GraphComponentDataIO.edata
) =
struct
  structure Theory = Theory;

  structure IVDataInputJSON = GraphComponentDataIO.IVDataInputJSON;
  structure IVDataOutputJSON = GraphComponentDataIO.IVDataOutputJSON;
  structure VDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = Theory.Graph.vdata;
    fun output Theory.Graph.WVert =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
          )
      | output (Theory.Graph.NVert d) =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
                           |> update ("data",IVDataOutputJSON.output d)
          )
  end;
  structure VDataInputJSON : INPUT_JSON =
  struct
    type T = Json.json
    exception bad_input_exp of string * string
    type data = Theory.Graph.vdata;
    fun input (json as Json.Object obj) = (
          case Json.lookup obj "is_wire_vertex"
            of SOME (Json.Bool false) => Theory.Graph.NVert (IVDataInputJSON.input json)
             | _ => Theory.Graph.WVert
        )
      | input _ = raise bad_input_exp ("expected: JSON object","0")
  end;
  structure EDataInputJSON = GraphComponentDataIO.EDataInputJSON;
  structure EDataOutputJSON = GraphComponentDataIO.EDataOutputJSON;

  structure RulesetAnnotations = RulesetStringTableAnnotations;
  structure GraphAnnotations = GraphStringTableAnnotations;

  structure InputGraphJSON = InputAnnotatedGraphJSON(
    structure Graph = Theory.Graph
    structure InputVertexData = GraphComponentDataIO.IVDataInputJSON
    structure InputEdgeData = GraphComponentDataIO.EDataInputJSON
    structure InputAnnotation = InputGraphStringTableAnnotationsJSON
  );
  structure OutputGraphJSON = OutputAnnotatedGraphJSON(
    structure Graph = Theory.Graph
    structure OutputVertexData = GraphComponentDataIO.IVDataOutputJSON
    structure OutputEdgeData = GraphComponentDataIO.EDataOutputJSON
    structure OutputAnnotation = OutputGraphStringTableAnnotationsJSON
  );
  structure OutputGraphDot = OutputGraphDot(
    structure Graph = Theory.Graph
    structure DotStyle = GraphComponentDataIO.DotStyle
  );
  structure InputRuleJSON = InputAnnotatedRuleJSON(
    structure Rule = Theory.Rule
    structure InputAnnotation = InputRulesetStringTableAnnotationsJSON.RuleAnnInput
    structure InputGraph = InputGraphJSON
  );
  structure OutputRuleJSON = OutputAnnotatedRuleJSON(
    structure Rule = Theory.Rule
    structure OutputAnnotation = OutputRulesetStringTableAnnotationsJSON.RuleAnnOutput
    structure OutputGraph = OutputGraphJSON
  );
  structure OutputRewriteJSON = OutputAnnotatedRewriteJSON(
    structure Rule = Theory.Rule
    structure OutputAnnotation = OutputRulesetStringTableAnnotationsJSON.RuleAnnOutput
    structure OutputGraph = OutputGraphJSON
  );
  structure OutputRewriteListJSON = OutputAnnotatedRewriteListJSON(
    structure Rule = Theory.Rule
    structure OutputAnnotation = OutputRulesetStringTableAnnotationsJSON
    structure OutputRewrite = OutputRewriteJSON
  );
  structure InputRulesetJSON = InputAnnotatedRulesetJSON(
    structure Ruleset = Theory.Ruleset
    structure InputAnnotation = InputRulesetStringTableAnnotationsJSON
    structure InputRule = InputRuleJSON
  );
  structure OutputRulesetJSON = OutputAnnotatedRulesetJSON(
    structure Ruleset = Theory.Ruleset
    structure OutputAnnotation = OutputRulesetStringTableAnnotationsJSON
    structure OutputRule = OutputRuleJSON
  );
end;

functor GraphicalTheoryIO (
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = GraphComponentDataIO.nvdata
  sharing type Theory.Graph.edata = GraphComponentDataIO.edata
) =
struct
  structure Theory = Theory;

  structure IVDataInputJSON = GraphComponentDataIO.IVDataInputJSON;
  structure IVDataOutputJSON = GraphComponentDataIO.IVDataOutputJSON;
  structure VDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = Theory.Graph.vdata;
    fun output Theory.Graph.WVert =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
          )
      | output (Theory.Graph.NVert d) =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool false)
                           |> update ("data",IVDataOutputJSON.output d)
          )
  end;
  structure EDataInputJSON = GraphComponentDataIO.EDataInputJSON;
  structure EDataOutputJSON = GraphComponentDataIO.EDataOutputJSON;

  structure InputGraphJSON = InputGraphJSON(
    structure Graph = Theory.Graph
    structure InputVertexData = GraphComponentDataIO.IVDataInputJSON
    structure InputEdgeData = GraphComponentDataIO.EDataInputJSON
  );
  structure OutputGraphJSON = OutputGraphJSON(
    structure Graph = Theory.Graph
    structure OutputVertexData = GraphComponentDataIO.IVDataOutputJSON
    structure OutputEdgeData = GraphComponentDataIO.EDataOutputJSON
  );
  structure OutputGraphDot = OutputGraphDot(
    structure Graph = Theory.Graph
    structure DotStyle = GraphComponentDataIO.DotStyle
  );
  structure InputRuleJSON = InputRuleJSON(
    structure Rule = Theory.Rule
    structure InputGraph = InputGraphJSON
  );
  structure OutputRuleJSON = OutputRuleJSON(
    structure Rule = Theory.Rule
    structure OutputGraph = OutputGraphJSON
  );
  structure OutputRewriteJSON = OutputRewriteJSON(
    structure Rule = Theory.Rule
    structure OutputGraph = OutputGraphJSON
  );
  structure OutputRewriteListJSON = OutputListJSON(
    structure OutputValue = OutputRewriteJSON
  );
  structure InputRulesetJSON = InputRulesetJSON(
    structure Ruleset = Theory.Ruleset
    structure InputRule = InputRuleJSON
  );
  structure OutputRulesetJSON = OutputRulesetJSON(
    structure Ruleset = Theory.Ruleset
    structure OutputRule = OutputRuleJSON
  );
end;

