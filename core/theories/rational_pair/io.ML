(* Data input for red-green graphs. The input strives to be as backward-compatible
 * as possible, while output always outputs the newest format. As a consequence,
 * the old Quantomatic GUI will no longer talk to the core. *)

structure RP_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = RP_Data.nvdata
  type edata = RP_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    structure L = InputLinratJSON
    type data = nvdata
    fun get_expr obj =
      case Json.lookup obj "value"
        of SOME (Json.String s) => 
             (LinratExpr.parse s
               handle LinratExpr.parse_exp =>
                 raise bad_input_exp ("Could not parse \""^s^"\"",""))
         | NONE => LinratExpr.zero
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "b" => RP_Data.Bnd LinratExpr.zero
              | "w" => RP_Data.Wnd LinratExpr.zero
              | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
      | input (Json.Object obj) =
          (case to_lower (get_string obj "type")
             of "b" => RP_Data.Bnd (get_expr obj)
              | "w" => RP_Data.Wnd (get_expr obj)
              | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected object","")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    structure L = OutputLinratJSON
    type data = nvdata
    fun expr_to_jstring a = Json.String
      (case Pretty.string_of (LinratExpr.pretty a) of "0" => "" | s => s)
    fun output (RP_Data.Bnd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "B")
                           |> update ("value",expr_to_jstring a)
          )
      | output (RP_Data.Wnd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "W")
                           |> update ("value",expr_to_jstring a)
          )
  end
  structure EDataInputJSON = InputUnitJSON
  structure EDataOutputJSON = OutputUnitJSON

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data (RP_Data.Bnd _) =
          "[style=filled,fillcolor=black,fontcolor=white,shape=circle]"
      | style_for_ivertex_data (RP_Data.Wnd _) =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
  end
end


