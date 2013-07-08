(* Generated using theories/generate-no-data-io.py Petri petri COPY SWITCH BUF0 BUF1 *)
structure Petri_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = Petri_Data.nvdata
  type edata = Petri_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun get_type t =
          (case to_lower t
             of "copy"   => Petri_Data.COPY
              | "switch" => Petri_Data.SWITCH
              | "buf0"   => Petri_Data.BUF0
              | "buf1"   => Petri_Data.BUF1
              | _      => raise bad_input_exp ("Unknown vertex type "^t,""))
    fun input (Json.String t) = get_type t
      | input (Json.Object obj) =
         (get_type (get_string obj "type")
            handle bad_input_exp (m,l) =>
              raise bad_input_exp (m, prepend_prop "type" l))
      | input _ = raise bad_input_exp ("Expected string","type")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = nvdata
    fun typestr Petri_Data.COPY   = "COPY"
      | typestr Petri_Data.SWITCH = "SWITCH"
      | typestr Petri_Data.BUF0   = "BUF0"
      | typestr Petri_Data.BUF1   = "BUF1"
    fun output d = Json.mk_record [("type",typestr d)]
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

