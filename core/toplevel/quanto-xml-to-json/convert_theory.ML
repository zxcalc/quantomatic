functor ConvertTheory(
  structure Theory : GRAPHICAL_THEORY
  structure GraphComponentDataIO : GRAPH_COMPONENT_DATA_IO
  structure InputVData : INPUT_XML
  structure InputEData : INPUT_XML
  sharing type Theory.Graph.nvdata = GraphComponentDataIO.nvdata
  sharing type Theory.Graph.edata = GraphComponentDataIO.edata
  sharing type Theory.Graph.vdata = InputVData.data
  sharing type Theory.Graph.edata = InputEData.data
)
= struct
  structure InputGraph = InputGraphXML_v2(
    structure Graph = Theory.Graph
          and InputVertexData = InputVData
          and InputEdgeData = InputEData
    )
  structure OutputGraph = OutputAnnotatedGraphJSON(
    structure Graph = Theory.Graph
    structure OutputVertexData = GraphComponentDataIO.IVDataOutputJSON
    structure OutputEdgeData = GraphComponentDataIO.EDataOutputJSON
    structure OutputAnnotation = OutputGraphStringTableAnnotationsJSON
  )
  structure GraphAnnInput = InputGraphStringTableAnnotationsXML

  fun convert_graph xml =
    OutputGraph.output (InputGraph.input xml,GraphAnnInput.input xml)

  structure InputRule = InputRuleXML(
    structure Rule = Theory.Rule
    structure InputGraph = InputGraph
  )
  structure OutputRule = OutputRuleJSON(
    structure Rule = Theory.Rule
    structure OutputGraph = OutputGraphJSON(
      structure Graph = Theory.Graph
      structure OutputVertexData = GraphComponentDataIO.IVDataOutputJSON
      structure OutputEdgeData = GraphComponentDataIO.EDataOutputJSON
    )
  )

  val convert_rule = OutputRule.output o InputRule.input

  structure InputRuleset = InputRulesetXML(
    structure Ruleset = Theory.Ruleset
    structure InputRule = InputRule
  )
  structure OutputRuleset = OutputAnnotatedRulesetJSON(
    structure Ruleset = Theory.Ruleset
    structure OutputAnnotation = OutputRulesetStringTableAnnotationsJSON
    structure OutputRule = OutputAnnotatedRuleJSON(
      structure Rule = Theory.Rule
      structure OutputAnnotation = OutputRulesetStringTableAnnotationsJSON.RuleAnnOutput
      structure OutputGraph = OutputGraph
    )
  )
  structure RulesetAnnInput = InputRulesetStringTableAnnotationsXML

  fun convert_ruleset xml =
    OutputRuleset.output (InputRuleset.input xml,RulesetAnnInput.input xml)
end

functor InputSimpleEnumData(Graph : BANG_GRAPH): INPUT_XML =
struct
  open XML_Parse_Utils
  type T = XML.tree
  type data = Graph.vdata
  fun input xml =
    let
      val () = assert_tag "vertex" xml
      val children = get_children xml
      val typename = get_unique_child_text (get_unique_elem "type" children)
    in
      if typename = "edge-point" then Graph.WVert
      else Graph.default_vdata_of_typestring typename
           handle unknown_typestring_exp t =>
             raise bad_input_exp ("Unknown typestring "^t,"type")
    end
end

structure GHZW_Converter = ConvertTheory(
  structure Theory = GHZW_Theory
  structure GraphComponentDataIO = GHZW_ComponentDataIO
  structure InputVData = InputSimpleEnumData(GHZW_Theory.Graph)
  structure InputEData = InputNoData
)

structure Petri_Converter = ConvertTheory(
  structure Theory = Petri_Theory
  structure GraphComponentDataIO = Petri_ComponentDataIO
  structure InputVData = InputSimpleEnumData(Petri_Theory.Graph)
  structure InputEData = InputNoData
)

structure RG_Converter
= struct
  structure InputVData : INPUT_XML =
  struct
    open XML_Parse_Utils
    type T = XML.tree
    structure Graph = RG_Theory.Graph
    type data = Graph.vdata
    fun input xml =
      let
        val () = assert_tag "vertex" xml
        val children = get_children xml
        val typename = get_unique_child_text (get_unique_elem "type" children)
        val to_lower = String.implode o (map Char.toLower) o String.explode
        fun get_angle () = let
          val data_el = get_unique_elem "data" children
        in
          InputLinratXML.input (get_unique_elem "angleexpr" (get_children data_el))
          handle bad_input_exp (m,s) => raise bad_input_exp (m,"data/"^s)
        end
      in
        case to_lower typename
          of "edge-point" => Graph.WVert
           | "hadamard" => Graph.NVert RG_Data.Hnd
           | "h" => Graph.NVert RG_Data.Hnd
           | "z" => Graph.NVert (RG_Data.Znd (get_angle()))
           | "x" => Graph.NVert (RG_Data.Xnd (get_angle()))
           | _ => raise bad_input_exp ("Unknown typestring "^typename,"type")
      end
  end
  structure GraphAnn = GraphStringTableAnnotations
  structure Conv_v2 = ConvertTheory(
    structure Theory = RG_Theory
    structure GraphComponentDataIO = RG_ComponentDataIO
    structure InputVData = InputVData
    structure InputEData = InputNoData
  )
  open Conv_v2

  fun convert_graph_v1 xml =
    OutputGraph.output (Input_RG_Graph_XML_v1.input xml,GraphAnn.init)
  val convert_graph_v2 = Conv_v2.convert_graph
  fun convert_graph xml = convert_graph_v2 xml
    handle e => (convert_graph_v1 xml handle _ => raise e)

  structure InputRule_v1 = InputRuleXML(
    structure Rule = RG_Theory.Rule
    structure InputGraph = Input_RG_Graph_XML_v1
  )

  val convert_rule_v1 = OutputRule.output o InputRule_v1.input
  val convert_rule_v2 = Conv_v2.convert_rule
  fun convert_rule xml = convert_rule_v2 xml
    handle e => (convert_rule_v1 xml handle _ => raise e)

  structure RulesetAnn = RulesetStringTableAnnotations
  structure InputRuleset_v1 = InputRulesetXML(
    structure Ruleset = RG_Theory.Ruleset
    structure InputRule = InputRule_v1
  )

  fun convert_ruleset_v1 xml =
    OutputRuleset.output (InputRuleset_v1.input xml,RulesetAnn.init)
  val convert_ruleset_v2 = Conv_v2.convert_ruleset
  fun convert_ruleset xml = convert_ruleset_v2 xml
    handle e => (convert_ruleset_v1 xml handle _ => raise e)
end

structure RGB_Converter = ConvertTheory(
  structure Theory = RGB_Theory
  structure GraphComponentDataIO = RGB_ComponentDataIO
  structure InputVData = InputSimpleEnumData(RGB_Theory.Graph)
  structure InputEData = InputNoData
)

structure RTechn_Converter
= struct
  structure InputVData : INPUT_XML =
  struct
    open XML_Parse_Utils
    type T = XML.tree
    structure Graph = RTechn_Theory.Graph
    type data = Graph.vdata
    fun input xml =
      let
        val () = assert_tag "vertex" xml
        val children = get_children xml
        val typename = get_unique_child_text (get_unique_elem "type" children)
      in
        case get_unique_child_text (get_unique_elem "type" children)
          of "edge-point" => Graph.WVert
           | "RT" =>
               let val data_el = get_unique_elem "data" children in
                 Graph.NVert (RTechn_Data.RTechnNode
                  (get_unique_child_text (get_unique_child data_el)))
                 handle bad_input_exp (m,s) => raise bad_input_exp (m,"data/"^s)
               end
           | "Gs" => (* nothing useful was ever stored here *)
               Graph.NVert (RTechn_Data.GoalNode ("",StrIntName.NSet.empty))
           | _ => raise bad_input_exp ("Unknown typestring "^typename,"type")
      end
  end
  structure InputEData : INPUT_XML =
  struct
    open XML_Parse_Utils
    type T = XML.tree
    structure Graph = RTechn_Theory.Graph
    type data = Graph.edata
    fun input xml =
      let
        val () = assert_tag "edge" xml
        val data_el = get_unique_elem "data" (get_children xml)
      in
        get_unique_child_text (get_unique_child data_el)
        handle _ => "" (* seems to always be empty *)
      end
  end
  structure Conv = ConvertTheory(
    structure Theory = RTechn_Theory
    structure GraphComponentDataIO = RTechn_ComponentDataIO
    structure InputVData = InputVData
    structure InputEData = InputEData
  )
  open Conv
end

structure Substrings_Converter
= struct
  structure InputVData : INPUT_XML =
  struct
    open XML_Parse_Utils
    type T = XML.tree
    structure Graph = Substrings_Theory.Graph
    type data = Graph.vdata
    fun input xml =
      let
        val () = assert_tag "vertex" xml
        val children = get_children xml
        val typename = get_unique_child_text (get_unique_elem "type" children)
      in
        if typename = "edge-point" then Graph.WVert
        else
          let val data_el = get_unique_elem "data" children in
            Graph.NVert (get_unique_child_text (get_unique_child data_el))
            handle bad_input_exp (m,s) => raise bad_input_exp (m,"data/"^s)
          end
      end
  end
  structure Conv = ConvertTheory(
    structure Theory = Substrings_Theory
    structure GraphComponentDataIO = Substrings_ComponentDataIO
    structure InputVData = InputVData
    structure InputEData = InputNoData
  )
  open Conv
end

structure Substr_Linrat_Converter
= struct
  structure InputVData : INPUT_XML =
  struct
    open XML_Parse_Utils
    type T = XML.tree
    structure Graph = Substr_Linrat_Theory.Graph
    type data = Graph.vdata
    fun input xml =
      let
        val () = assert_tag "vertex" xml
        val children = get_children xml
        val typename = get_unique_child_text (get_unique_elem "type" children)
        fun get_angle () = let
          val data_el = get_unique_elem "data" children
        in
          InputLinratXML.input (get_unique_elem "angleexpr" (get_children data_el))
          handle bad_input_exp (m,s) => raise bad_input_exp (m,"data/"^s)
        end
      in
        if typename = "edge-point" then Graph.WVert
        (* the string data was never stored in the XML graphs *)
        else Graph.NVert ("",get_angle())
      end
  end
  structure GraphAnn = GraphStringTableAnnotations
  structure Conv = ConvertTheory(
    structure Theory = Substr_Linrat_Theory
    structure GraphComponentDataIO = Substr_Linrat_ComponentDataIO
    structure InputVData = InputVData
    structure InputEData = InputNoData
  )
  open Conv
end

(* vim:et:sts=2:sw=2
*)
