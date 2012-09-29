structure Petri_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type vdata = Petri_Data.data;
  type edata = unit;

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils;
    type data = Petri_Data.data;
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "buf0"   => Petri_Data.BUF0
              | "buf1"   => Petri_Data.BUF1
              | "copy"   => Petri_Data.COPY
              | "switch" => Petri_Data.SWITCH
              | _        => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected string","type");
  end;
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    type data = Petri_Data.data;
    fun output Petri_Data.BUF0   = Json.String "buf0"
      | output Petri_Data.BUF1   = Json.String "buf1"
      | output Petri_Data.COPY   = Json.String "copy"
      | output Petri_Data.SWITCH = Json.String "switch"
  end;
  structure EDataInputJSON = InputUnitJSON;
  structure EDataOutputJSON = OutputUnitJSON;

  structure DotStyle : DOT_STYLE =
  struct
    type vdata = vdata;
    fun style_for_ivertex_data Petri_Data.BUF0   =
          "[style=filled,fillcolor=white,fontcolor=black,shape=square]"
      | style_for_ivertex_data Petri_Data.BUF1   =
          "[style=filled,fillcolor=black,fontcolor=white,shape=square]"
      | style_for_ivertex_data Petri_Data.COPY   =
          "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
      | style_for_ivertex_data Petri_Data.SWITCH =
          "[style=filled,fillcolor=blue,fontcolor=black,shape=circle]";
  end;
end;

(* Use this for convenience if you don't need annotations *)
structure Petri_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = Petri_Theory
  structure GraphComponentDataIO = Petri_ComponentDataIO
);

