structure StringVE_InputJSON : INPUT_JSON =
struct
  open JsonInputUtils
  type data = StringData.data
  fun input (Json.Object obj) =
        (case Json.lookup obj "value"
           of SOME(Json.String(s)) => s
            | _  => raise bad_input_exp ("Expected: field \"value\" containing string", "type"))
    | input _ = raise bad_input_exp ("Expected object","type")
end

structure StringVE_OutputJSON : OUTPUT_JSON =
struct
  open JsonOutputUtils
  type data = StringData.data
  fun output s = Json.mk_record [("type", "string"), ("value", s)]
end

structure StringVE_GraphComponentDataIO: GRAPH_COMPONENT_DATA_IO =
struct
  type vdata = StringData.data
  type edata = StringData.data

  structure IVDataInputJSON = StringVE_InputJSON
  structure IVDataOutputJSON = StringVE_OutputJSON
  structure EDataInputJSON = StringVE_InputJSON
  structure EDataOutputJSON = StringVE_OutputJSON
  structure DotStyle =
  struct
    type vdata = vdata
    fun style_for_ivertex_data _ =
      "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
  end
end

structure StringVE_IO = JsonObjectAnnotatedGraphicalTheoryIO(
  structure Theory = StringVE_Theory
  structure GraphComponentDataIO = StringVE_GraphComponentDataIO
)