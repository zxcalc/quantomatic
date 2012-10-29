structure SUBSTR_LINRAT_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type vdata = SUBSTR_LINRAT_InternVData.data;
  type edata = unit;

  structure IVDataInputJSON : INPUT_JSON =
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
  structure IVDataOutputJSON : OUTPUT_JSON =
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
  structure EDataInputJSON = InputUnitJSON;
  structure EDataOutputJSON = OutputUnitJSON;

  structure DotStyle : DOT_STYLE =
  struct
    type vdata = vdata;
    fun style_for_ivertex_data _ =
          "[style=filled,fillcolor=green,fontcolor=black,shape=circle]";
  end;
end;

(* Use this for convenience if you don't need annotations *)
structure SUBSTR_LINRAT_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = SUBSTR_LINRAT_Theory
  structure GraphComponentDataIO = SUBSTR_LINRAT_ComponentDataIO
);

