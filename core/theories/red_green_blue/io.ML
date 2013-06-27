structure RGB_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = RGB_Data.nvdata
  type edata = RGB_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "red"   => RGB_Data.Red
              | "green" => RGB_Data.Green
              | "blue"  => RGB_Data.Blue
              | _       => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected string","type")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = nvdata
    fun output RGB_Data.Red = Json.String "Red"
      | output RGB_Data.Green = Json.String "Green"
      | output RGB_Data.Blue = Json.String "Blue"
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data RGB_Data.Red   =
          "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
      | style_for_ivertex_data RGB_Data.Green =
          "[style=filled,fillcolor=green,fontcolor=white,shape=circle]"
      | style_for_ivertex_data RGB_Data.Blue  =
          "[style=filled,fillcolor=blue,fontcolor=white,shape=circle]"
  end
end

(* Use this for convenience if you don't need annotations *)
structure RGB_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = RGB_Theory
  structure GraphComponentDataIO = RGB_ComponentDataIO
)

