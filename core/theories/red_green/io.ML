structure RG_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = RG_Data.nvdata
  type edata = RG_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    structure L = InputLinratJSON
    type data = nvdata
    fun get_angle obj =
      case Json.lookup obj "value"
        of SOME v => L.input v
         | NONE => L.input (get_easy Json.Null obj "angle")
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "hadamard" => RG_Data.Hnd
              | "h" => RG_Data.Hnd
              | "x" => RG_Data.Xnd LinratAngleExpr.zero
              | "z" => RG_Data.Znd LinratAngleExpr.zero
              | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
      | input (Json.Object obj) =
          (case to_lower (get_string obj "type")
             of "hadamard" => RG_Data.Hnd
              | "h" => RG_Data.Hnd
              | "z" => RG_Data.Znd (get_angle obj)
              | "x" => RG_Data.Xnd (get_angle obj)
              | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected object","")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    structure L = OutputLinratJSON
    type data = nvdata
    val angle_to_jstring = Json.String o Pretty.string_of o LinratAngleExpr.pretty
    fun output (RG_Data.Znd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "Z")
                           |> update ("label",angle_to_jstring a)
                           |> update ("value",L.output a)
          )
      | output (RG_Data.Xnd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "X")
                           |> update ("label",angle_to_jstring a)
                           |> update ("value",L.output a)
          )
      | output RG_Data.Hnd =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "hadamard")
          )
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data (RG_Data.Znd _) =
          "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
      | style_for_ivertex_data (RG_Data.Xnd _) =
          "[style=filled,fillcolor=red,fontcolor=white,shape=circle]"
      | style_for_ivertex_data RG_Data.Hnd =
          "[style=filled,fillcolor=yellow,fontcolor=white,shape=square]"
  end
end

(* Use this for convenience if you don't need annotations *)
structure RG_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = RG_Theory
  structure GraphComponentDataIO = RG_ComponentDataIO
)

