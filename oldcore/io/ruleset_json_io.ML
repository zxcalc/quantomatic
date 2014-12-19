signature RULESET_JSON =
sig
  structure Ruleset : RULESET
  val input : Json.json -> Ruleset.T
  val output : Ruleset.T -> Json.json
end

functor RulesetJSON(
  structure Ruleset : RULESET
  structure RuleJSON : RULE_JSON
  sharing RuleJSON.Rule.Sharing = Ruleset.Rule.Sharing
) : RULESET_JSON
= struct

structure Ruleset = Ruleset

open JsonInputUtils
open JsonOutputUtils

fun map_rule (n,v) tab = let
  val rule = RuleJSON.input v
    handle bad_input_exp (m,l) =>
      raise bad_input_exp (m,prepend_prop n l)
in
  RTab.add (R.mk n,rule) tab
end

fun input_rules obj prop ruleset =
      case get_easy Json.Null obj prop
        of Json.Object obj =>
            let
              val rules = (Json.fold map_rule obj RTab.empty
                                  handle bad_input_exp (m,l) =>
                                    raise bad_input_exp (m,prepend_prop prop l))
            in
              Ruleset.set_allrules rules ruleset
            end
         | Json.Null => ruleset
         | _ => raise bad_input_exp ("Expected object","");

fun map_tag (n,(Json.Array a)) =
      let
        fun map_rulename (Json.String s) = R.mk s
          | map_rulename _ = raise bad_input_exp ("Expected strings in array","")
        val tag = TagName.mk n
      in
        fold (fn r => RTagRel.add (r,tag)) (map map_rulename a)
      end
  | map_tag _ = raise bad_input_exp ("Expected array","")

fun input_tags obj prop =
      case get_easy Json.Null obj prop
        of Json.Object obj =>
            let
              val tags = (Json.fold map_tag obj RTagRel.empty
                         handle bad_input_exp (m,l) =>
                           raise bad_input_exp (m,prepend_prop prop l))
            in
              Ruleset.set_brel tags
            end
         | Json.Null => I
         | _ => raise bad_input_exp ("Expected object","");

fun input_active obj prop = let
  fun map_rulename (Json.String s) = R.mk s
    | map_rulename _ = raise bad_input_exp ("Expected strings in array","")
  val a = get_array_easy obj prop
in
  Ruleset.set_active (R.NSet.of_list (map map_rulename a))
end

fun input (Json.Object obj) =
      Ruleset.empty
          |> input_rules obj "rules"
          |> input_tags obj "tags"
          |> input_active obj "active_rules"
  | input Json.Null = Ruleset.empty
  | input _ = raise bad_input_exp ("Expected object","");

val rule_list = Json.Array o (map (Json.String o R.dest)) o R.NSet.list_of;

fun output_rules rs = let
  val ruletab = Ruleset.get_allrules rs
  fun upd_rule (r,v) = update (R.dest r, RuleJSON.output v)
in
  Json.Object (RTab.fold upd_rule ruletab Json.empty_obj)
end

fun output_tags rs = let
  val tagrel = Ruleset.get_tagrel rs
  fun upd_tag t = update (TagName.dest t, rule_list (RTagRel.inv_img tagrel t))
in
  Json.Object (TagName.NSet.fold upd_tag (RTagRel.get_cod_set tagrel) Json.empty_obj)
end

val output_active_rules = rule_list o Ruleset.get_active;

fun output rs =
  Json.Object (
    Json.empty_obj |> update ("rules",output_rules rs)
                   |> update ("tags",output_tags rs)
                   |> update ("active_rules",output_active_rules rs)
  )

end