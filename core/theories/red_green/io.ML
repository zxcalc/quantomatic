structure RG_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type vdata = RG_InternVData.data;
  type edata = unit;

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils;
    structure L = InputLinratJSON;
    type data = RG_InternVData.data;
    val get_angle = L.input oo (get_easy Json.Null)
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "h" => RG_InternVData.Hnd
              | "x" => RG_InternVData.Xnd LinratAngleExpr.zero
              | "z" => RG_InternVData.Znd LinratAngleExpr.zero
              | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
      | input (Json.Object obj) =
          (case to_lower (get_string obj "type")
             of "h" => RG_InternVData.Hnd
              | "z" => RG_InternVData.Znd (get_angle obj "angle")
              | "x" => RG_InternVData.Xnd (get_angle obj "angle")
              | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected object","");
  end;
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils;
    structure L = OutputLinratJSON;
    type data = RG_InternVData.data;
    fun output (RG_InternVData.Znd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "Z")
                           |> update ("angle",L.output a)
          )
      | output (RG_InternVData.Xnd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "X")
                           |> update ("angle",L.output a)
          )
      | output RG_InternVData.Hnd = Json.String "H"
  end;
  structure EDataInputJSON = InputUnitJSON;
  structure EDataOutputJSON = OutputUnitJSON;

  structure DotStyle : DOT_STYLE =
  struct
    type vdata = vdata;
    fun style_for_ivertex_data (RG_InternVData.Znd _) =
          "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
      | style_for_ivertex_data (RG_InternVData.Xnd _) =
          "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
      | style_for_ivertex_data RG_InternVData.Hnd =
          "[style=filled,fillcolor=yellow,fontcolor=white,shape=square]";
  end;
end;

(* Use this for convenience if you don't need annotations *)
structure RG_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = RG_Theory
  structure GraphComponentDataIO = RG_ComponentDataIO
);

