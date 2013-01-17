signature GRAPH_COMPONENT_DATA_IO =
sig
  type vdata;
  type edata;

  structure IVDataInputJSON : INPUT_JSON
    where type data = vdata;
  structure IVDataOutputJSON : OUTPUT_JSON
    where type data = vdata;
  structure EDataInputJSON : INPUT_JSON
    where type data = edata;
  structure EDataOutputJSON : OUTPUT_JSON
    where type data = edata;
  structure DotStyle : DOT_STYLE
    where type vdata = vdata;
end;

functor StringTableAnnotatedGraphicalTheoryIO (
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.OVData.IData.data = GraphComponentDataIO.vdata
  sharing type Theory.EData.data = GraphComponentDataIO.edata
) =
struct
  structure Theory = Theory;

  structure IVDataInputJSON = GraphComponentDataIO.IVDataInputJSON;
  structure IVDataOutputJSON = GraphComponentDataIO.IVDataOutputJSON;
  structure VDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = Theory.OVData.data;
    fun output Theory.OVData.WVert =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
          )
      | output (Theory.OVData.NVert d) =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
                           |> update ("data",IVDataOutputJSON.output d)
          )
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

functor JsonObjectAnnotatedGraphicalTheoryIO (
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.OVData.IData.data = GraphComponentDataIO.vdata
  sharing type Theory.EData.data = GraphComponentDataIO.edata
) =
struct
  structure Theory = Theory;

  structure IVDataInputJSON = GraphComponentDataIO.IVDataInputJSON;
  structure IVDataOutputJSON = GraphComponentDataIO.IVDataOutputJSON;
  structure VDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = Theory.OVData.data;
    fun output Theory.OVData.WVert =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
          )
      | output (Theory.OVData.NVert d) =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
                           |> update ("data",IVDataOutputJSON.output d)
          )
  end;
  structure EDataInputJSON = GraphComponentDataIO.EDataInputJSON;
  structure EDataOutputJSON = GraphComponentDataIO.EDataOutputJSON;

  structure RulesetAnnotations = RulesetJsonObjectAnnotations;
  structure GraphAnnotations = GraphJsonObjectAnnotations;

  structure InputGraphJSON = InputAnnotatedGraphJSON(
    structure Graph = Theory.Graph
    structure InputVertexData = GraphComponentDataIO.IVDataInputJSON
    structure InputEdgeData = GraphComponentDataIO.EDataInputJSON
    structure InputAnnotation = InputGraphJsonObjectAnnotationsJSON
  );
  structure OutputGraphJSON = OutputAnnotatedGraphJSON(
    structure Graph = Theory.Graph
    structure OutputVertexData = GraphComponentDataIO.IVDataOutputJSON
    structure OutputEdgeData = GraphComponentDataIO.EDataOutputJSON
    structure OutputAnnotation = OutputGraphJsonObjectAnnotationsJSON
  );
  structure OutputGraphDot = OutputGraphDot(
    structure Graph = Theory.Graph
    structure DotStyle = GraphComponentDataIO.DotStyle
  );
  structure InputRuleJSON = InputAnnotatedRuleJSON(
    structure Rule = Theory.Rule
    structure InputAnnotation = InputRulesetJsonObjectAnnotationsJSON.RuleAnnInput
    structure InputGraph = InputGraphJSON
  );
  structure OutputRuleJSON = OutputAnnotatedRuleJSON(
    structure Rule = Theory.Rule
    structure OutputAnnotation = OutputRulesetJsonObjectAnnotationsJSON.RuleAnnOutput
    structure OutputGraph = OutputGraphJSON
  );
  structure OutputRewriteJSON = OutputAnnotatedRewriteJSON(
    structure Rule = Theory.Rule
    structure OutputAnnotation = OutputRulesetJsonObjectAnnotationsJSON.RuleAnnOutput
    structure OutputGraph = OutputGraphJSON
  );
  structure OutputRewriteListJSON = OutputAnnotatedRewriteListJSON(
    structure Rule = Theory.Rule
    structure OutputAnnotation = OutputRulesetJsonObjectAnnotationsJSON
    structure OutputRewrite = OutputRewriteJSON
  );
  structure InputRulesetJSON = InputAnnotatedRulesetJSON(
    structure Ruleset = Theory.Ruleset
    structure InputAnnotation = InputRulesetJsonObjectAnnotationsJSON
    structure InputRule = InputRuleJSON
  );
  structure OutputRulesetJSON = OutputAnnotatedRulesetJSON(
    structure Ruleset = Theory.Ruleset
    structure OutputAnnotation = OutputRulesetJsonObjectAnnotationsJSON
    structure OutputRule = OutputRuleJSON
  );
end;

functor GraphicalTheoryIO (
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.OVData.IData.data = GraphComponentDataIO.vdata
  sharing type Theory.EData.data = GraphComponentDataIO.edata
) =
struct
  structure Theory = Theory;

  structure IVDataInputJSON = GraphComponentDataIO.IVDataInputJSON;
  structure IVDataOutputJSON = GraphComponentDataIO.IVDataOutputJSON;
  structure VDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = Theory.OVData.data;
    fun output Theory.OVData.WVert =
          Json.Object (
            Json.empty_obj |> update ("is_wire_vertex",Json.Bool true)
          )
      | output (Theory.OVData.NVert d) =
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

