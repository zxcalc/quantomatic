
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
  structure OutputRGGraphXML : OUTPUT_XML
    = OutputGraphXML (
      structure Graph = RGGraph
      and OutputVertexData = OutputRGVertexDataXMLList(RGGraph.RGVertex)
      and OutputEdgeData = OutputNoEdgeDataXMLList(RGGraph.Edge)
  );

  structure OutputRGRuleXML : OUTPUT_XML
   = OutputRuleXML(
      structure Rule = RGRule
      and OutputGraph = OutputRGGraphXML
     );

  structure OutputRGRewriteXML 
  : OUTPUT_XML where type data = (RuleName.name *RGRule.T) * RGGraph.T
   = OutputRewriteXML(
      structure Rule = RGRule
      and OutputGraph = OutputRGGraphXML
     );

  structure OutputRGRewriteListXML 
  : OUTPUT_XML where type data = ((RuleName.name * RGRule.T) * RGGraph.T) list
   = OutputWrapXML(
      val tagname = "rewrites"
      val attrs = []
      structure Output1 = OutputMap(OutputRGRewriteXML)
     );

  structure OutputRGRulesetXML : OUTPUT_XML
   = OutputRulesetXML(
      structure Ruleset = RGRuleset
      structure OutputRule = OutputRulePartsXMLList(
        structure Rule = RGRule
        and OutputGraph = OutputRGGraphXML
      )
     );

in val _ = (); end;
