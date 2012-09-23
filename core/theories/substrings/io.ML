structure Str_DataInputJSON : INPUT_JSON =
struct
  open JsonInputUtils;
  type data = PrefixStringVertexData.data;
  fun input (Json.String str) = str
    | input _ = raise bad_input_exp ("Expected string","type");
end;

structure Str_DataOutputJSON : OUTPUT_JSON =
struct
  open JsonOutputUtils;
  type data = PrefixStringVertexData.data;
  val output = Json.String;
end;

structure Str_GraphInputJSON : INPUT_JSON =
  InputGraphJSON(
    structure Graph = Str_Theory.Graph
    structure InputVertexData = Str_DataInputJSON
    structure InputEdgeData = InputUnitJSON
  );

structure Str_GraphOutputJSON : OUTPUT_JSON =
  OutputGraphJSON(
    structure Graph = Str_Theory.Graph
    structure OutputVertexData = Str_DataOutputJSON
    structure OutputEdgeData = OutputUnitJSON
  );


structure Str_DotStyle : DOT_STYLE =
struct
  structure Graph = Str_Theory.Graph
  fun style_for_ivertex_data _ = "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
end;

structure Str_OutputGraphDot = OutputGraphDot(structure DotStyle = Str_DotStyle)
