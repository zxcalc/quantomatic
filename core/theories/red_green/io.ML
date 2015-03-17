(* Data input for red-green graphs. The input strives to be as backward-compatible
 * as possible, while output always outputs the newest format. As a consequence,
 * the old Quantomatic GUI will no longer talk to the core. *)

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
           (* new RG-graphs just store angle as a string, to be parsed *)
        of SOME (Json.String s) => 
             (LinratAngleExpr.parse s
               handle LinratAngleExpr.parse_exp =>
                 raise bad_input_exp ("Could not parse \""^s^"\"",""))
           (* older RG-graphs use a (redundant) JSON representation of angle data *)
         | SOME (v as Json.Object _) => L.input v
           (* really old (pre-Derive) graphs call this field 'angle' *)
         | NONE => L.input (get_easy Json.Null obj "angle")
         | _ => raise bad_input_exp ("Expected string or object for 'value' of red or green node","")
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun input (Json.String t) =
          (case to_lower t
             of "hadamard" => RG_Data.Hnd
              | "h" => RG_Data.Hnd
              | "x" => RG_Data.Xnd LinratAngleExpr.zero
              | "z" => RG_Data.Znd LinratAngleExpr.zero
              | "var" => RG_Data.Var ""
              | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
      | input (Json.Object obj) =
          (case to_lower (get_string obj "type")
             of "hadamard" => RG_Data.Hnd
              | "h" => RG_Data.Hnd
              | "z" => RG_Data.Znd (get_angle obj)
              | "x" => RG_Data.Xnd (get_angle obj)
              | "var" => RG_Data.Var (case Json.lookup obj "value"
                                        of SOME (Json.String s) => s
                                         | NONE => ""
                                         | _ => raise bad_input_exp ("Expected string for 'value' of variable",""))
              | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected object","")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    structure L = OutputLinratJSON
    type data = nvdata
    fun angle_to_jstring a = Json.String
      (case Pretty.string_of (LinratAngleExpr.pretty a) of "0" => "" | s => s)
    fun output (RG_Data.Znd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "Z")
                           |> update ("value",angle_to_jstring a)
          )
      | output (RG_Data.Xnd a) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "X")
                           |> update ("value",angle_to_jstring a)
          )
      | output RG_Data.Hnd =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "hadamard")
          )
      | output (RG_Data.Var s) =
          Json.Object (
            Json.empty_obj |> update ("type", Json.String "var")
                           |> update ("value", Json.String s)
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
      | style_for_ivertex_data (RG_Data.Var _) =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
      | style_for_ivertex_data RG_Data.Hnd =
          "[style=filled,fillcolor=yellow,fontcolor=white,shape=square]"
  end
end


