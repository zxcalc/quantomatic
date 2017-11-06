signature RULE_JSON =
sig
  structure Rule : BANG_GRAPH_RULE
  val input : Json.json -> Rule.T
  val output : Rule.T -> Json.json
end

functor RuleJSON(
  structure Rule : BANG_GRAPH_RULE
  structure GraphJSON : GRAPH_JSON
  sharing GraphJSON.Graph.Sharing = Rule.Graph.Sharing
) : RULE_JSON
= struct

structure Rule = Rule

open JsonInputUtils
open JsonOutputUtils

fun input (Json.Object obj) =
      let
        val lhs = GraphJSON.input (get obj "lhs")
                    handle bad_input_exp (m,l) =>
                      raise bad_input_exp (m,prepend_prop "lhs" l)
        val rhs = GraphJSON.input (get obj "rhs")
                    handle bad_input_exp (m,l) =>
                      raise bad_input_exp (m,prepend_prop "rhs" l)
      in
        Rule.mk (lhs,rhs)
          |> (case Json.lookup obj "annotation"
                of SOME (Json.Object ann) => Rule.set_rule_annotation ann
                 | SOME Json.Null => I
                 | SOME _ => raise bad_input_exp ("Annotation must be object or null","")
                 | NONE => I)
        handle Rule.bad_rule_exp (msg,_,_) =>
          raise bad_input_exp ("Invalid rule: "^msg,"")
      end
  | input _ = raise bad_input_exp ("Expected object","")

fun output r =
      Json.Object (
        Json.empty_obj |> update ("lhs",(GraphJSON.output (Rule.get_lhs r)))
                       |> update ("rhs",(GraphJSON.output (Rule.get_rhs r)))
                       |> (if Json.is_empty_obj (Rule.get_rule_annotation r)
                           then update ("annotation", Json.mk_object [("layout", Json.Bool true)])
                           else update ("annotation", Json.Object (Rule.get_rule_annotation r)))
      )

end
