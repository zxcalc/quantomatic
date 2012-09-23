structure Substr_Linrat_DataInputJSON : INPUT_JSON =
struct
  open JsonInputUtils;
  type data = SUBSTR_LINRAT_InternVData.data;
  structure L = InputLinratJSON;
  val get_angle = L.input oo (get_easy Json.Null)
  fun input (Json.Object obj) =
        let
          val str = get_string_easy "" obj "string"
          val angle = get_angle obj "angle"
        in
          SUBSTR_LINRAT_InternVData.S_E (str,angle)
        end
    | input _ = raise bad_input_exp ("Expected string","type");
end;

structure Substr_Linrat_DataOutputJSON : OUTPUT_JSON =
struct
  open JsonOutputUtils;
  structure L = OutputLinratJSON;
  type data = SUBSTR_LINRAT_InternVData.data;
  fun output (SUBSTR_LINRAT_InternVData.S_E (str,angle)) =
        Json.Object (
          Json.empty_obj |> Json.update ("string",Json.String str)
                         |> Json.update ("angle",L.output angle)
        )
end;

structure Substr_Linrat_GraphInputJSON : INPUT_JSON =
  InputGraphJSON(
    structure Graph = SUBSTR_LINRAT_Theory.Graph
    structure InputVertexData = Substr_Linrat_DataInputJSON
    structure InputEdgeData = InputUnitJSON
  );

structure Substr_Linrat_GraphOutputJSON : OUTPUT_JSON =
  OutputGraphJSON(
    structure Graph = SUBSTR_LINRAT_Theory.Graph
    structure OutputVertexData = Substr_Linrat_DataOutputJSON
    structure OutputEdgeData = OutputUnitJSON
  );


structure SUBSTR_LINRAT_DotStyle : DOT_STYLE =
struct
  structure Graph = SUBSTR_LINRAT_Theory.Graph
  fun style_for_ivertex_data _ = "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
end;

structure SUBSTR_LINRAT_OutputGraphDot = OutputGraphDot(structure DotStyle = SUBSTR_LINRAT_DotStyle)

