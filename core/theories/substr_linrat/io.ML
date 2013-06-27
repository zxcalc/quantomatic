structure Substr_Linrat_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = Substr_Linrat_Data.nvdata
  type edata = Substr_Linrat_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    structure L = InputLinratJSON
    val get_angle = L.input oo (get_easy Json.Null)
    fun input (Json.Object obj) =
          let
            val str = get_string_easy "" obj "string"
            val angle = get_angle obj "angle"
          in
            (str,angle)
          end
      | input _ = raise bad_input_exp ("Expected string","type")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    structure L = OutputLinratJSON
    type data = nvdata
    fun output (str,angle) =
          Json.Object (
            Json.empty_obj |> Json.update ("string",Json.String str)
                           |> Json.update ("angle",L.output angle)
          )
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data _ =
          "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
  end
end

(* Use this for convenience if you don't need annotations *)
structure Substr_Linrat_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = Substr_Linrat_Theory
  structure GraphComponentDataIO = Substr_Linrat_ComponentDataIO
)

