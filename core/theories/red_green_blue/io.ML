(* Generated using theories/generate-no-data-io.py RGB red_green_blue Red Green Blue *)
structure RGB_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = RGB_Data.nvdata
  type edata = RGB_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun get_type t =
          (case to_lower t
             of "red"   => RGB_Data.Red
              | "green" => RGB_Data.Green
              | "blue"  => RGB_Data.Blue
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
    fun typestr RGB_Data.Red   = "Red"
      | typestr RGB_Data.Green = "Green"
      | typestr RGB_Data.Blue  = "Blue"
    fun output d = Json.mk_record [("type",typestr d)]
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


