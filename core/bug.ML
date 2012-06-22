val fr_xml = "<?xml version=\"1.0\"?>\n<rule><lhs><graph><vertex name=\"f\"><type>GHZ</type><data/></vertex><vertex name=\"e\"><type>GHZ</type><data/></vertex><vertex name=\"d\"><type>edge-point</type><data/></vertex><vertex name=\"c\"><type>edge-point</type><data/></vertex><vertex name=\"b\"><type>edge-point</type><data/></vertex><vertex name=\"a\"><type>edge-point</type><data/></vertex><edge name=\"e\" dir=\"true\" source=\"e\" target=\"f\"><type>unit</type><data/></edge><edge name=\"d\" dir=\"true\" source=\"f\" target=\"d\"><type>unit</type><data/></edge><edge name=\"c\" dir=\"true\" source=\"c\" target=\"f\"><type>unit</type><data/></edge><edge name=\"b\" dir=\"true\" source=\"e\" target=\"b\"><type>unit</type><data/></edge><edge name=\"a\" dir=\"true\" source=\"a\" target=\"e\"><type>unit</type><data/></edge><bangbox name=\"Ba\"><vertex>a</vertex></bangbox><bangbox name=\"Bb\"><vertex>b</vertex></bangbox><bangbox name=\"Bc\"><vertex>c</vertex></bangbox><bangbox name=\"Bd\"><vertex>d</vertex></bangbox></graph></lhs><rhs><graph><vertex name=\"g\"><type>GHZ</type><data/></vertex><vertex name=\"d\"><type>edge-point</type><data/></vertex><vertex name=\"c\"><type>edge-point</type><data/></vertex><vertex name=\"b\"><type>edge-point</type><data/></vertex><vertex name=\"a\"><type>edge-point</type><data/></vertex><edge name=\"i\" dir=\"true\" source=\"g\" target=\"d\"><type>unit</type><data/></edge><edge name=\"h\" dir=\"true\" source=\"c\" target=\"g\"><type>unit</type><data/></edge><edge name=\"g\" dir=\"true\" source=\"g\" target=\"b\"><type>unit</type><data/></edge><edge name=\"f\" dir=\"true\" source=\"a\" target=\"g\"><type>unit</type><data/></edge><bangbox name=\"Ba\"><vertex>a</vertex></bangbox><bangbox name=\"Bb\"><vertex>b</vertex></bangbox><bangbox name=\"Bc\"><vertex>c</vertex></bangbox><bangbox name=\"Bd\"><vertex>d</vertex></bangbox></graph></rhs></rule>"
val sp_xml = "<?xml version=\"1.0\"?><graph><vertex name=\"c\"><type>GHZ</type><data/></vertex><vertex name=\"b\"><type>edge-point</type><data/></vertex><vertex name=\"a\"><type>edge-point</type><data/></vertex><edge name=\"c\" dir=\"true\" source=\"c\" target=\"c\"><type>unit</type><data/></edge><edge name=\"b\" dir=\"true\" source=\"c\" target=\"b\"><type>unit</type><data/></edge><edge name=\"a\" dir=\"true\" source=\"a\" target=\"c\"><type>unit</type><data/></edge><bangbox name=\"Ba\"><vertex>a</vertex></bangbox><bangbox name=\"Bb\"><vertex>b</vertex></bangbox></graph>"

val fr = GHZW_Theory.IO_Xml.Input.Rule.input (XMLReader.read_from_string fr_xml)
val sp = GHZW_Theory.IO_Xml.Input.Graph.input (XMLReader.read_from_string sp_xml)

fun rule_matches_graph r g = let
    val lhs = GHZW_Theory.Ruleset.Rule.get_lhs r
    val gs = [lhs, g]
    val names = 
        (fold GHZW_Theory.Graph.add_to_vnames  gs GHZW_Theory.Graph.Vertex.NSet.empty,
         fold GHZW_Theory.Graph.add_to_enames  gs GHZW_Theory.Graph.Edge.NSet.empty,
         fold GHZW_Theory.Graph.add_to_bboxes gs GHZW_Theory.Graph.BBox.NSet.empty)
val matches = GHZW_Theory.RuleMatchSearch.match names r g
in case Seq.pull matches of NONE=>false | _=>true
end

val result = rule_matches_graph fr sp
  
  