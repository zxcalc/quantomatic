(* Code to output a rewrite tree: they represent a lot of data but
 * they are used in an algorithm which adds nodes at each step but never
 * removes any. Moreover, existing nodes are never renamed : we can keep
 * the list of names and output only new graphs*)

  functor OutputVerticesXMLList(
    structure RGraph : RGRAPH
    structure VertexData : ELEMENT_COMPONENT_DATA;
    structure EdgeData : ELEMENT_COMPONENT_DATA; 
    sharing type VertexData.data = RGraph.Graph.VData.data
    sharing type EdgeData.data = RGraph.Graph.EData.data
  ) = struct

    structure RGraph = RGraph;
    structure outgraph =  OutputGraphXML_v2(
      structure Graph = RGraph.Graph;
      structure OutputVertexData = OutputTypedDataXML(VertexData) 
      structure OutputEdgeData = OutputTypedDataXML(EdgeData)
    );

    type T = XML.tree list;
    type data = RGraph.T * (GraphName.name list); 

    open XML;
    structure Util = XML_Output_Utils;
    (* Output only the names which are not in the list *) 
    fun output (rg, names) =
      let
        val to_output = GraphName.NSet.subtract (RGraph.get_vnames rg)
          (GraphName.NSet.of_list names)
                |> GraphName.NSet.subtract (RGraph.get_vnames rg)
                |> GraphName.NSet.list_of

        fun add_to_ntab (n,v) tab = if List.exists 
          (fn x => case GraphName.name_ord (x,n) of EQUAL => true | _ => false) to_output
          then GraphName.NTab.ins (n, fst v) tab
          else tab
        val ntab = GraphName.NTab.fold (add_to_ntab) (RGraph.get_vertices rg)
          GraphName.NTab.empty
      in
        map (fn (n,v) => Util.wrap_els_simple "rewritten_graph" 
                          [Util.wrap_text_simple "graphname" (GraphName.dest n),
                           Util.wrap_el_simple "the_graph" (outgraph.output
                           v)]) (GraphName.NTab.list_of ntab)
      end;
  end;

  functor OutputEdgeXMLList(
    structure RGraph : RGRAPH
  ) : OUTPUT_XML_LIST
  = struct
    type T = XML.tree list;
    type edge_type = RGraph.edge_type;
    type edge_data = RGraph.edge_data;
    type data = ((GraphName.name * GraphName.name) * edge_data) * edge_type

    open XML;
    structure Util = XML_Output_Utils;
    (* Simply output : Rulename, source, target and type *)
    fun output_rulename d = Util.wrap_text_simple "rulename" 
      (R.dest ((fst o snd o fst) d));
    fun output_src d = Util.wrap_text_simple "source"
      (GraphName.dest ((fst o fst o fst) d));
    fun output_tgt d = Util.wrap_text_simple "target"
      (GraphName.dest ((snd o fst o fst) d));
    fun output_type d = Util.wrap_text_simple "type" 
      (RGraph.edge_type_to_string (snd d));

    fun output d = [output_rulename d]@[output_src d]@[output_tgt d]@
                    [output_type d];
  end;

  functor OutputRGraphXMLList(
    structure RTree : REWRITE_TREE
  ) : OUTPUT_XML_LIST
  = struct

    structure RGraph = RTree.RewriteGraph;

    structure edgesout = OutputNTabXMLList (
      val entry_tagname = "edge" 
      val key_tagname = "edgename"
      val value_tagname = "rulename"
      structure NTab = E.NTab
      structure OutputName = OutputListify(OutputSStrNameXML(E)):OUTPUT_XML_LIST
        where type data = E.name
      structure OutputVal = OutputEdgeXMLList(structure RGraph = RGraph)
    );
    structure vout = OutputVerticesXMLList(structure RGraph = RGraph
                                           structure VertexData =
                                             RTree.GTheory.VertexComponentData
                                           structure EdgeData = 
                                             RTree.GTheory.EdgeComponentData);

    type T = XML.tree list;
    type data = RGraph.T * (GraphName.name list);

    open XML;
    structure Util = XML_Output_Utils;
    fun root_output rg = case RGraph.get_root rg
      of SOME n => Util.wrap_text_simple "root" (GraphName.dest n)
       | NONE => Util.wrap_text_simple "root" "none" ;
    val edges_out = edgesout.output o RGraph.get_edges;

    fun output (rg, p) = [root_output rg]@(vout.output (rg,p))@
                            (edges_out rg);
  end;


  functor OutputRewriteTreeXMLList (
    structure RTree : REWRITE_TREE
  ) : OUTPUT_XML_LIST
  = struct
    structure rgout = OutputRGraphXMLList(structure RTree = RTree);

    structure agendaout = OutputWrapXML (
      val tagname = "agenda"
      val attrs = []
      structure Output1 = OutputListXMLList (
        val item_tagname = "item"
        structure Output1 = OutputListify(OutputSStrNameXML(GraphName)))
      );

    type T = XML.tree list;
    type data = RTree.T * (GraphName.name list);

    open XML;
    structure Util = XML_Output_Utils;
    val output_agenda = agendaout.output o RTree.get_agenda;
    fun output_current rt = case RTree.get_current_vertex rt
      of SOME n =>  Util.wrap_text_simple "current" (GraphName.dest n) 
       | NONE => Util.wrap_text_simple "current" "none";

    fun output (rt, previous) = [output_current rt]@[output_agenda rt]
      @(rgout.output (RTree.get_rgraph rt, previous));
  end;

  functor OutputRewriteTreeXML (
    structure RTree : REWRITE_TREE
  ) : OUTPUT_XML
  = OutputWrapXML (
    val tagname = "rewrite_tree"
    val attrs = []
    structure Output1 = OutputRewriteTreeXMLList (
      structure RTree = RTree : REWRITE_TREE
    )
  );
