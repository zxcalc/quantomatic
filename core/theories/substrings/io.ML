structure Substrings_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = Substrings_Data.nvdata
  type edata = Substrings_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    fun input (Json.Object obj) = get_string_easy "" obj "value"
      | input (Json.String str) = str
      | input _ = raise bad_input_exp ("Expected string","type")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = nvdata
    fun output s = Json.mk_record [("type", "string"), ("label",s), ("value", s)]
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data _ =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
  end
end



