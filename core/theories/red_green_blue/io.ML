structure RGB_VertexDataInputJSON : INPUT_JSON =
struct
  open JsonInputUtils;
  type data = RGB_VertexData.data;
  val to_lower = String.implode o (map Char.toLower) o String.explode
  fun input (Json.String t) =
        (case to_lower t
           of "red"   => RGB_VertexData.Red
            | "green" => RGB_VertexData.Green
            | "blue"  => RGB_VertexData.Blue
            | _       => raise bad_input_exp ("Unknown vertex type "^t,"type"))
    | input _ = raise bad_input_exp ("Expected string","type");
end;

structure RGB_VertexDataOutputJSON : OUTPUT_JSON =
struct
  open JsonOutputUtils;
  type data = RGB_VertexData.data;
  fun output RGB_VertexData.Red = Json.String "red"
    | output RGB_VertexData.Green = Json.String "green"
    | output RGB_VertexData.Blue = Json.String "blue"
end;

structure RGB_GraphInputJSON : INPUT_JSON =
  InputGraphJSON(
    structure Graph = RGB_Theory.Graph
    structure InputVertexData = RGB_VertexDataInputJSON
    structure InputEdgeData = InputUnitJSON
  );

structure RGB_GraphOutputJSON : OUTPUT_JSON =
  OutputGraphJSON(
    structure Graph = RGB_Theory.Graph
    structure OutputVertexData = RGB_VertexDataOutputJSON
    structure OutputEdgeData = OutputUnitJSON
  );

structure RGB_DotStyle : DOT_STYLE =
struct
  structure Graph = RGB_Theory.Graph
  fun style_for_ivertex_data RGB_VertexData.Red   = "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
    | style_for_ivertex_data RGB_VertexData.Green = "[style=filled,fillcolor=green,fontcolor=white,shape=circle]"
    | style_for_ivertex_data RGB_VertexData.Blue  = "[style=filled,fillcolor=blue,fontcolor=white,shape=circle]"
    
end

structure RGB_OutputGraphDot = OutputGraphDot(structure DotStyle = RGB_DotStyle)
