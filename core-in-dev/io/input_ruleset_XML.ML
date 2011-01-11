
signature INPUT_RULESET =
sig
  structure InputRule : INPUT_RULE
  (* structure RuleSet : RULESET *)
  include INPUT
    where type intype = InputRule.intype (* XML, etc. *)
      and type outtype = (InputRule.Rule.T * bool) InputRule.Rule.Name.NTab.T
          (* outtype will = RuleSet.T once we can generify Rule (no spider hack) *)
end;

(* TODO: functor-ify *)
structure RGRuleSetInputXML : INPUT_RULESET where type intype = XML.tree =
struct
  structure InputRule = RGRuleInputXML
  structure Rule = InputRule.Rule
  (* structure RuleSet = RuleSet *)
  val input_rule = InputRule.input
  
  type intype = XML.tree
  type outtype = (Rule.T * bool) Rule.Name.NTab.T (* will = RuleSet.T *)
  
  open XML_Utils
  
  fun input xml = 
      let
        val (_,_,children) = force_unpack_elem "ruleset" xml
        fun add_rule ch rset = let
          val (nm, rl, active) = input_rule ch
        in snd (rset |> Rule.Name.NTab.add (nm, (rl, active)))
        end
      in
        fold add_rule (get_all_elems "rule" children) Rule.Name.NTab.empty
      end
end;

