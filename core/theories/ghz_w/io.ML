(* Generated using theories/generate-no-data-io.py GHZW ghzw GHZ W TICK ZERO *)
structure GHZW_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = GHZW_Data.nvdata
  type edata = GHZW_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun get_type t =
          (case to_lower t
             of "ghz"  => GHZW_Data.GHZ
              | "w"    => GHZW_Data.W
              | "tick" => GHZW_Data.TICK
              | "zero" => GHZW_Data.ZERO
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
    fun typestr GHZW_Data.GHZ  = "GHZ"
      | typestr GHZW_Data.W    = "W"
      | typestr GHZW_Data.TICK = "TICK"
      | typestr GHZW_Data.ZERO = "ZERO"
    fun output d = Json.mk_record [("type",typestr d)]
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data GHZW_Data.GHZ =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
      | style_for_ivertex_data GHZW_Data.W =
          "[style=filled,fillcolor=black,fontcolor=white,shape=circle]"
      | style_for_ivertex_data GHZW_Data.TICK =
          "[style=filled,fillcolor=red,fontcolor=white,shape=diamond]"
      | style_for_ivertex_data GHZW_Data.ZERO =
          "[style=filled,fillcolor=blue,fontcolor=black,shape=square]"
  end
end


