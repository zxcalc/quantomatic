
(* ---------------------------------------------------------- *)
(*  TESTING INPUT *)
local 
  structure InputRGGraphXML : INPUT_XML
    = InputGraphXML(
        structure Graph = RGGraph
        and InputVertexData = InputRGVertexDataXML(RGGraph.RGVertex)
        and InputEdgeData = InputNoEdgeDataXML(RGGraph.Edge)
      );
  
  (* ------------------------------------------------------- *)
  structure InputRGRuleXML : INPUT_XML
    = InputRuleXML(
        structure Rule = RGRule
        and InputGraph = InputRGGraphXML
  );

  structure InputRGRulesetXML : INPUT_XML where type data = RGRuleset.T 
    = InputRulesetXML(
        structure Ruleset = RGRuleset
        structure InputRule = InputRGRuleXML
        structure InputRuleName = InputRuleNameXML
        structure InputTagName = InputTagNameXML); 

in val _ = (); end;


(*-------------------------------------------------------*)
(* TESTING OUTPUT *)
local 
  structure OutputRGGraphXML : OUTPUT_PRETTY_XML
    = OutputGraphXML (
      structure Graph = RGGraph
      and OutputVertexData = OutputRGVertexDataXML(RGGraph.RGVertex)
      and OutputEdgeData = OutputNoEdgeDataXML(RGGraph.Edge)
  );

  structure OutputRGRuleXML : OUTPUT_PRETTY_XML
   = OutputRuleXML(
      structure Rule = RGRule
      and OutputGraph = OutputRGGraphXML
     );

  structure OutputRGRewriteXML 
  : OUTPUT_PRETTY_XML where type data = (RuleName.name *RGRule.T) * RGGraph.T
   = OutputRewriteXML(
      structure Rule = RGRule
      and OutputGraph = OutputRGGraphXML
     );

  structure OutputRGRewriteListXML 
  : OUTPUT_PRETTY_XML where type data = ((RuleName.name * RGRule.T) * RGGraph.T) list
   = OutputListXML(
      val tagname = "rewrites"
      structure Output1 = OutputRGRewriteXML
     );

  structure OutputRGRulesetXML : OUTPUT_PRETTY_XML
   = OutputRulesetXML(
      structure Ruleset = RGRuleset
      structure OutputRule = OutputRGRuleXML
     );

in val _ = (); end;
