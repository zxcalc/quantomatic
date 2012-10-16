structure RGB_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type vdata = RGB_VertexData.data;
  type edata = unit;

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils;
    type data = RGB_VertexData.data;
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "red"   => RGB_VertexData.Red
              | "green" => RGB_VertexData.Green
              | "blue"  => RGB_VertexData.Blue
              | _       => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected string","type");
  end;
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = RGB_VertexData.data;
    fun output RGB_VertexData.Red = Json.String "Red"
      | output RGB_VertexData.Green = Json.String "Green"
      | output RGB_VertexData.Blue = Json.String "Blue"
  end;
  structure EDataInputJSON = InputUnitJSON;
  structure EDataOutputJSON = OutputUnitJSON;

  structure DotStyle : DOT_STYLE =
  struct
    type vdata = vdata;
    fun style_for_ivertex_data RGB_VertexData.Red   =
          "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
      | style_for_ivertex_data RGB_VertexData.Green =
          "[style=filled,fillcolor=green,fontcolor=white,shape=circle]"
      | style_for_ivertex_data RGB_VertexData.Blue  =
          "[style=filled,fillcolor=blue,fontcolor=white,shape=circle]";
  end;
end;

(* Use this for convenience if you don't need annotations *)
structure RGB_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = RGB_Theory
  structure GraphComponentDataIO = RGB_ComponentDataIO
);

