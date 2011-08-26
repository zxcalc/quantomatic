let
  val exampleGraphData = "<?xml version=\"1.0\"?><graph><vertex name=\"h\"><type>edge-point</type><data/></vertex><vertex name=\"g\"><type>edge-point</type><data/></vertex><vertex name=\"f\"><type>edge-point</type><data/></vertex><vertex name=\"e\"><type>hadamard</type><data/></vertex><vertex name=\"d\"><type>Z</type><data><angleexpr><string_of>(1/2) x + iy</string_of><variable><name>iy</name><coefficient><num>1</num><denom>1</denom></coefficient></variable><variable><name>x</name><coefficient><num>1</num><denom>2</denom></coefficient></variable></angleexpr></data></vertex><vertex name=\"c\"><type>Z</type><data><angleexpr><string_of>\\pi</string_of><constant><num>1</num><denom>1</denom></constant></angleexpr></data></vertex><vertex name=\"b\"><type>X</type><data><angleexpr><string_of>0</string_of></angleexpr></data></vertex><vertex name=\"a\"><type>X</type><data><angleexpr><string_of>\\alpha</string_of><variable><name>\\alpha</name><coefficient><num>1</num><denom>1</denom></coefficient></variable></angleexpr></data></vertex><edge name=\"h\" dir=\"true\" source=\"d\" target=\"h\"><type>unit</type><data/></edge><edge name=\"g\" dir=\"true\" source=\"a\" target=\"g\"><type>unit</type><data/></edge><edge name=\"f\" dir=\"true\" source=\"f\" target=\"c\"><type>unit</type><data/></edge><edge name=\"e\" dir=\"true\" source=\"e\" target=\"a\"><type>unit</type><data/></edge><edge name=\"d\" dir=\"true\" source=\"e\" target=\"d\"><type>unit</type><data/></edge><edge name=\"c\" dir=\"true\" source=\"c\" target=\"e\"><type>unit</type><data/></edge><edge name=\"b\" dir=\"false\" source=\"b\" target=\"a\"><type>unit</type><data/></edge><edge name=\"a\" dir=\"false\" source=\"c\" target=\"a\"><type>unit</type><data/></edge><bangbox name=\"Ba\"><vertex>b</vertex></bangbox><bangbox name=\"Bb\"><vertex>d</vertex><vertex>e</vertex></bangbox><bangbox name=\"Bc\"/></graph>"

  fun vertexName (n,_,_) = n
  fun vertexType (_,t,_) = t
  fun vertexData (_,_,d) = d
  fun eq a b = a = b
  fun the (SOME x) = x
  val exampleGraphVertices = [("a","X",SOME "\\alpha"),
                              ("b","X",SOME "0"),
                              ("c","Z",SOME "\\pi"),
                              ("d","Z",SOME "(1/2) x + iy"),
                              ("e","hadamard",NONE),
                              ("f","edge-point",NONE),
                              ("g","edge-point",NONE),
                              ("h","edge-point",NONE)]
  val exampleGraphVertexNames = map vertexName exampleGraphVertices
  fun exampleGraphVertex vn = the (List.find ((eq vn) o vertexName) exampleGraphVertices)
  val exampleGraphVertexType = vertexType o exampleGraphVertex
  val exampleGraphVertexData = vertexData o exampleGraphVertex

  fun edgeName   (n,_,_,_,_,_) = n
  fun edgeIsDir  (_,d,_,_,_,_) = d
  fun edgeSource (_,_,s,_,_,_) = s
  fun edgeTarget (_,_,_,t,_,_) = t
  fun edgeType   (_,_,_,_,t,_) = t
  fun edgeData   (_,_,_,_,_,d) = d
  val exampleGraphEdges = [("a",false,"c","a","unit",NONE),
                           ("b",false,"b","a","unit",NONE),
                           ("c",true ,"e","e","unit",NONE),
                           ("d",true ,"e","d","unit",NONE),
                           ("e",true ,"e","a","unit",NONE),
                           ("f",true ,"f","c","unit",NONE),
                           ("g",true ,"a","g","unit",NONE),
                           ("h",true ,"d","h","unit",NONE)]
  val exampleGraphEdgeNames = map edgeName exampleGraphEdges
  fun exampleGraphEdge en = the (List.find ((eq en) o edgeName) exampleGraphEdges)
  val exampleGraphEdgeIsDir = edgeIsDir o exampleGraphEdge
  val exampleGraphEdgeSource = edgeSource o exampleGraphEdge
  val exampleGraphEdgeTarget = edgeTarget o exampleGraphEdge
  val exampleGraphEdgeType = edgeType o exampleGraphEdge
  val exampleGraphEdgeData = edgeData o exampleGraphEdge

  fun bangBoxName     (b,_) = b
  fun bangBoxVertices (_,v) = v
  val exampleGraphBangBoxes = [("Ba",["b"]),
                               ("Bb",["d","e"]),
                               ("Bc",[])]
  val exampleGraphBangBoxNames = map bangBoxName exampleGraphBangBoxes
  fun exampleGraphBangBox bn = the (List.find ((eq bn) o bangBoxName) exampleGraphBangBoxes)
  val exampleGraphBangBoxVertices = bangBoxVertices o exampleGraphBangBox
in

registerTest "Add/list/kill graphs" (fn session => (
let
  fun checkGraphNames expNames =
    let
      val _ = writeRequestByParts session ("GL","santa's little helper") ""
      val graphNames = readNameListResponse session "santa's little helper"
    in
      if unordered_eq graphNames expNames then () else
        raise test_exp "Got wrong graph names"
    end

  val _ = writeRequestByParts session ("GL","homer") ""
  val _ = demandNameListResponse session "homer" []

  val _ = writeRequestByParts session ("GOE","lisa") ""
  val gr1Name = readNameResponse session "lisa"

  val _ = writeRequestByParts session ("GOE","marge") "magic-graph"
  val _ = demandNameResponse session "marge" "magic-graph"

  val _ = writeRequestByParts session ("GOE","bart") "magic-graph"
  val gr3Name = readNameResponse session "bart"
  val _ = if "magic-graph" <> gr3Name then () else
      raise test_exp "GOE overwrote an existing graph!"

  val _ = checkGraphNames [gr1Name,"magic-graph",gr3Name]

  val _ = writeRequestByArgs session ("GR","barney") ["magic-graph","ordinary graph"]
  val _ = demandNameResponse session "barney" "ordinary graph"

  val _ = checkGraphNames [gr1Name,"ordinary graph",gr3Name]

  val _ = writeRequestByArgs session ("GR","monty") ["ordinary graph",gr3Name]
  val gr2Name = readNameResponse session "monty"
  val _ = if gr2Name <> gr3Name then () else
      raise test_exp "GR overwrote an existing graph!"

  val _ = checkGraphNames [gr1Name,gr2Name,gr3Name]

  val _ = writeRequestByParts session ("GD","maggie") gr1Name
  val _ = demandOkResponse session "maggie"

  val _ = checkGraphNames [gr2Name,gr3Name]

  val _ = writeRequestByParts session ("GOG","moe") gr2Name
  val gr4Name = readNameResponse session "moe"

  val _ = checkGraphNames [gr2Name,gr3Name,gr4Name]
in () end
));

registerTest "Load/save graphs" (fn session => (
let
  val inputGraphFile = OS.FileSys.tmpName()
  val graphOutstream = TextIO.openOut inputGraphFile
  val _ = TextIO.output (graphOutstream,exampleGraphData)
  val _ = TextIO.closeOut graphOutstream
  val outputGraphFile = OS.FileSys.tmpName()
  val dummyGraphFile = OS.FileSys.tmpName()

  val _ = writeRequestByArgs session ("GOD","London") ["example",dataChunk exampleGraphData]
  val _ = demandNameResponse session "London" "example"

  val _ = writeRequestByArgs session ("GOD","Cardiff") ["example",dataChunk "gibberish"]
  val _ = demandErrorResponse session "Cardiff" "BADDATA"

  val _ = writeRequestByParts session ("GOF","Paris") inputGraphFile
  val graph2Name = readNameResponse session "Paris"

  val _ = writeRequestByParts session ("GOF","Dublin") dummyGraphFile
  val _ = demandErrorResponse session "Dublin" "BADDATA"

  val _ = OS.FileSys.remove dummyGraphFile
  val _ = writeRequestByParts session ("GOF","Madrid") dummyGraphFile
  val _ = demandErrorResponse session "Madrid" "FILEACCESS"

  val _ = writeRequestByArgs session ("GS","Helsinki") ["example","/proc/dummy.graph"]
  val _ = demandErrorResponse session "Helsinki" "FILEACCESS"

  val _ = writeRequestByArgs session ("GS","Stockholm") ["dummy graph",outputGraphFile]
  val _ = demandErrorResponse session "Stockholm" "NOSUCHGRAPH"

  val _ = writeRequestByArgs session ("GS","Berlin") ["example",outputGraphFile]
  val _ = demandOkResponse session "Berlin"

  val graphInstream = TextIO.openIn outputGraphFile
  val graphData2 = TextIO.inputAll graphInstream
  val _ = TextIO.closeIn graphInstream

  val _ = writeRequestByArgs session ("GE","Reykjavik") ["dummy graph","native"]
  val _ = demandErrorResponse session "Reykjavik" "NOSUCHGRAPH"

  val _ = writeRequestByArgs session ("GE","Copenhagen") ["example","dummy"]
  val _ = demandErrorResponse session "Copenhagen" "BADFORMAT"

  (* check that the saved data is the same as the exported data *)
  val _ = writeRequestByArgs session ("GE","Edinburgh") [graph2Name,"native"]
  val _ = demandDataResponse session "Edinburgh" graphData2

  val _ = OS.FileSys.remove inputGraphFile
  val _ = OS.FileSys.remove outputGraphFile
in () end
));

registerTest "Describe graphs" (fn session => (
let
  val _ = writeRequestByArgs session ("GOD","USA") ["example",dataChunk exampleGraphData]
  val _ = demandNameResponse session "USA" "example"

  val _ = writeRequestByArgs session ("GVVL","Canada") ["dummy graph"]
  val _ = demandErrorResponse session "Canada" "NOSUCHGRAPH"

  val _ = writeRequestByArgs session ("GVVL","Mexico") ["example"]
  val vertices = readNameListResponse session "Mexico"
  val _ = if unordered_eq vertices exampleGraphVertexNames then () else
        raise test_exp "Got wrong vertex names"

  val _ = writeRequestByArgs session ("GVEL","Bahamas") ["dummy graph"]
  val _ = demandErrorResponse session "Bahamas" "NOSUCHGRAPH"

  val _ = writeRequestByArgs session ("GVEL","Antigua and Barbuda") ["example"]
  val edges = readNameListResponse session "Antigua and Barbuda"
  val _ = if unordered_eq edges exampleGraphEdgeNames then () else
        raise test_exp "Got wrong edge names"

  val _ = writeRequestByArgs session ("GVBL","Barbados") ["dummy graph"]
  val _ = demandErrorResponse session "Barbados" "NOSUCHGRAPH"

  val _ = writeRequestByArgs session ("GVBL","Belize") ["example"]
  val bangBoxes = readNameListResponse session "Belize"
  val _ = if unordered_eq bangBoxes exampleGraphBangBoxNames then () else
        raise test_exp "Got wrong !-box names"
in () end
));

() end

(* vi:et:sw=2:sts=2
*)
