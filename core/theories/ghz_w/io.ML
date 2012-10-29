structure GHZW_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type vdata = GHZW_Data.data;
  type edata = unit;

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils;
    type data = GHZW_Data.data;
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "ghz"  => GHZW_Data.GHZ
              | "w"    => GHZW_Data.W
              | "tick" => GHZW_Data.TICK
              | "zero" => GHZW_Data.ZERO
              | _      => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected string","type");
  end;
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = GHZW_Data.data;
    fun output GHZW_Data.GHZ  = Json.String "GHZ"
      | output GHZW_Data.W    = Json.String "W"
      | output GHZW_Data.TICK = Json.String "tick"
      | output GHZW_Data.ZERO = Json.String "zero"
  end;
  structure EDataInputJSON = InputUnitJSON;
  structure EDataOutputJSON = OutputUnitJSON;

  structure DotStyle : DOT_STYLE =
  struct
    type vdata = vdata;
    fun style_for_ivertex_data GHZW_Data.GHZ =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
      | style_for_ivertex_data GHZW_Data.W =
          "[style=filled,fillcolor=black,fontcolor=white,shape=circle]"
      | style_for_ivertex_data GHZW_Data.TICK =
          "[style=filled,fillcolor=red,fontcolor=white,shape=diamond]"
      | style_for_ivertex_data GHZW_Data.ZERO =
          "[style=filled,fillcolor=blue,fontcolor=black,shape=square]";
  end;
end;

(* Use this for convenience if you don't need annotations *)
structure GHZW_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = GHZW_Theory
  structure GraphComponentDataIO = GHZW_ComponentDataIO
);

