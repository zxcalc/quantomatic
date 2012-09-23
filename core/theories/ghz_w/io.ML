structure GHZW_DataInputJSON : INPUT_JSON =
struct
  open JsonInputUtils;
  type data = GHZW_Data.data;
  val to_lower = String.implode o (map Char.toLower) o String.explode
  fun input (Json.String t) =
        (case to_lower t
           of "ghz"  => GHZW_Data.GHZ
            | "w"    => GHZW_Data.W
            | "tick" => GHZW_Data.TICK
            | "zero" => GHZW_Data.ZERO
            | _      => raise bad_input_exp ("Unknown vertex type "^t,"type"))
    | input _ = raise bad_input_exp ("Expected string","type");
end;

structure GHZW_DataOutputJSON : OUTPUT_JSON =
struct
  open JsonOutputUtils;
  type data = GHZW_Data.data;
  fun output GHZW_Data.GHZ  = Json.String "ghz"
    | output GHZW_Data.W    = Json.String "w"
    | output GHZW_Data.TICK = Json.String "tick"
    | output GHZW_Data.ZERO = Json.String "zero"
end;

structure GHZW_GraphInputJSON : INPUT_JSON =
  InputGraphJSON(
    structure Graph = GHZW_Theory.Graph
    structure InputVertexData = GHZW_DataInputJSON
    structure InputEdgeData = InputUnitJSON
  );

structure GHZW_GraphOutputJSON : OUTPUT_JSON =
  OutputGraphJSON(
    structure Graph = GHZW_Theory.Graph
    structure OutputVertexData = GHZW_DataOutputJSON
    structure OutputEdgeData = OutputUnitJSON
  );

structure GHZW_DotStyle : DOT_STYLE =
struct
  structure Graph = GHZW_Theory.Graph
  fun style_for_ivertex_data GHZW_Data.GHZ = "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
    | style_for_ivertex_data GHZW_Data.W = "[style=filled,fillcolor=black,fontcolor=white,shape=circle]"
    | style_for_ivertex_data GHZW_Data.TICK = "[style=filled,fillcolor=red,fontcolor=white,shape=diamond]"
    | style_for_ivertex_data GHZW_Data.ZERO = "[style=filled,fillcolor=blue,fontcolor=black,shape=square]"
end

structure GHZW_OutputGraphDot = OutputGraphDot(structure DotStyle = GHZW_DotStyle)
