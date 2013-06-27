structure Petri_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = Petri_Data.nvdata
  type edata = Petri_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "copy"   => Petri_Data.COPY
              | "switch" => Petri_Data.SWITCH
              | "buf0"   => Petri_Data.BUF0
              | "buf1"   => Petri_Data.BUF1
              | _      => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected string","type")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = nvdata
    fun output Petri_Data.COPY   = Json.String "COPY"
      | output Petri_Data.SWITCH = Json.String "SWITCH"
      | output Petri_Data.BUF0   = Json.String "BUF0"
      | output Petri_Data.BUF1   = Json.String "BUF1"
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data Petri_Data.BUF0   =
          "[style=filled,fillcolor=white,fontcolor=black,shape=square]"
      | style_for_ivertex_data Petri_Data.BUF1   =
          "[style=filled,fillcolor=black,fontcolor=white,shape=square]"
      | style_for_ivertex_data Petri_Data.COPY   =
          "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
      | style_for_ivertex_data Petri_Data.SWITCH =
          "[style=filled,fillcolor=blue,fontcolor=black,shape=circle]";
  end
end

(* Use this for convenience if you don't need annotations *)
structure Petri_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = Petri_Theory
  structure GraphComponentDataIO = Petri_ComponentDataIO
)

