structure StringVE_ComponentDataIO: GRAPH_COMPONENT_DATA_IO =
struct
  type nvdata = StringVE_Data.nvdata
  type edata = StringVE_Data.edata

  structure IVDataInputJSON =
  struct
    open JsonInputUtils
    type data = StringVE_Data.nvdata
    fun input (Json.Object obj) = get_string_easy "" obj "value"
      | input (Json.String s) = s
      | input _ = raise bad_input_exp ("Expected object","type")
  end

  structure IVDataOutputJSON =
  struct
    open JsonOutputUtils
    type data = StringVE_Data.nvdata
    fun output s = Json.mk_record [("type", "string"), ("label",s), ("value", s)]
  end

  structure EDataInputJSON = 
  struct
    open JsonInputUtils
    type data = StringVE_Data.edata
    fun input (Json.Object obj) = get_string_easy "" obj "value"
      | input (Json.String s) = s
      | input _ = raise bad_input_exp ("Expected object","type")
  end

  structure EDataOutputJSON = 
  struct
    open JsonOutputUtils
    type data = StringVE_Data.edata
    fun output s = Json.mk_record [("type", "string"), ("label",s), ("value", s)]
  end

  structure DotStyle =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data _ =
      "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
  end
end
