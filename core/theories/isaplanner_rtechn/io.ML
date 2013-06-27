structure RTechn_ComponentDataIO : GRAPH_COMPONENT_DATA_IO
= struct
  type nvdata = RTechn_Data.nvdata
  type edata = RTechn_Data.edata

  structure IVDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = nvdata
    val to_lower = String.implode o (map Char.toLower) o String.explode
    fun goal_name_from_json Json.Null set = set
      | goal_name_from_json (Json.Object obj) set =
          let
            val str = get_string_easy "" obj "string"
            val num = get_int_easy 0 obj "number"
          in
            StrIntName.NSet.ins_fresh (str,num) set
            handle StrIntName.NSet.duplicate_exp _ =>
              raise bad_input_exp
                ("goal name \""^str^"\","^
                 (Int.toString num)^" is a duplicate entry","")
          end
      | goal_name_from_json _ _ = raise bad_input_exp ("Expected object","")
    fun get_goal_names obj prop =
          fold_arr_easy goal_name_from_json obj prop StrIntName.NSet.empty
    fun input (Json.Object obj) =
          (case to_lower (get_string obj "type")
             of "RT" => RTechn_Data.RTechnNode
                              (get_string_easy "?" obj "tactic")
              | "Gs" =>
                  (RTechn_Data.GoalNode
                     (get_string_easy "" obj "goal_type",
                     get_goal_names obj "goal_names")
                     handle bad_input_exp (m,l) =>
                       raise bad_input_exp (m,prepend_prop "goal" l))
              | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
      | input _ = raise bad_input_exp ("Expected object","")
  end
  structure IVDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = nvdata
    fun output (RTechn_Data.RTechnNode n) =
          Json.Object (
            Json.empty_obj |> update ("type",Json.String "RT")
                           |> update ("tactic",Json.String n)
          )
      | output (RTechn_Data.GoalNode (n,gset)) =
          let
            fun to_json (str,i) =
              Json.Object (
                Json.empty_obj |> update ("string",Json.String str)
                               |> update ("number",Json.Int i)
              )
            val gns = map to_json (StrIntName.NSet.list_of gset)
          in
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "Gs")
                             |> update ("goal_type",Json.String n)
                             |> update ("goal_names",Json.Array gns)
            )
          end
  end
  structure EDataInputJSON : INPUT_JSON =
  struct
    open JsonInputUtils
    type data = edata
    fun input (Json.String str) = str
      | input _ = raise bad_input_exp ("Expected string","type")
  end
  structure EDataOutputJSON : OUTPUT_JSON =
  struct
    open JsonOutputUtils
    type data = edata
    val output = Json.String
  end

  structure DotStyle : DOT_STYLE =
  struct
    type nvdata = nvdata
    fun style_for_ivertex_data _ =
          "[style=filled,fillcolor=white,fontcolor=black,shape=circle]"
  end
end

(* Use this for convenience if you don't need annotations *)
structure RTechn_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = RTechn_Theory
  structure GraphComponentDataIO = RTechn_ComponentDataIO
)

