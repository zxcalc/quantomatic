structure Str_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type vdata = PrefixStringVertexData.data;
  type edata = unit;

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils;
    type data = PrefixStringVertexData.data;
    fun input (Json.String str) = str
      | input _ = raise bad_input_exp ("Expected string","type");
  end;
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = PrefixStringVertexData.data;
    val output = Json.String;
  end;
  structure EDataInputJSON = InputUnitJSON;
  structure EDataOutputJSON = OutputUnitJSON;

  structure DotStyle : DOT_STYLE =
  struct
    type vdata = vdata;
    fun style_for_ivertex_data _ =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]";
  end;
end;

(* Use this for convenience if you don't need annotations *)
structure Str_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = Str_Theory
  structure GraphComponentDataIO = Str_ComponentDataIO
);

