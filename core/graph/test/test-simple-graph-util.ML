(*This structure contains a number of functions to simplify dealing with graphs.
It abstracts away from some of the additional data within the graph.*)
structure SimpleGraphUtil =
struct
  local open Test_BBox_Util; in
  
  (*Construct a new empty graph.*)
  val empty = G.empty
  
  (*********************************)
  (******All kinds of vertices******)
  (*********************************)
  
  (*Return the name table for all of the node vertices*)
  fun get_vertices g = G.get_vertices g
  (*Get the name set for all of the vertices*)
  fun get_vertex_names g = V.NTab.get_nameset (get_vertices g)
  (*Return the list of all of the vertices in the graph*)
  fun get_list_vertex_names g = V.NSet.list_of (get_vertex_names g)
  (*Return a unique vertex name not in the graph...*)
  fun unique_V_name g = V.NSet.new (get_vertex_names g) (V.mk "a")
  (*Create a vertex "name" with the name given as a string*)
  fun create_vertex name = V.mk name
  (*Test to see if two vertices are equal*)
  fun vertex_eq v1 v2 = V.name_eq (v1,v2)
  (*Remove a vertex from the graph*)
  fun delete_vertex v g = G.delete_vertex v g
  (*Get the set of edges coming into this vertex*)
  fun get_inc v g =
    let
      val neighbourTable = G.get_vertices g;
      val res = V.NTab.lookup neighbourTable v; (*Get the adjacency matrix from the graph*)
    in
      case res of
          NONE => E.NSet.empty (*Return an empty set since the vertex might not exist*)
        | SOME (_,(incoming,_)) => incoming
    end
  (*Get the set of edges leaving this vertex*)
  fun get_adj v g =
    let
      val neighbourTable = G.get_vertices g;
      val res = V.NTab.lookup neighbourTable v; (*Get the adjacency matrix from the graph*)
    in
      case res of
          NONE => E.NSet.empty (*Return an empty set since the vertex might not exist*)
        | SOME (_,(_,outgoing)) => outgoing
    end
  (*Return the number of vertices in the graph*)
  fun vertex_count g = V.NSet.cardinality (get_vertex_names g)
  
  (*********************************)
  (********Node vertices only*******)
  (*********************************)
  
  (*Return the name table only for the node vertices*)
  fun get_node_vertex_names g = G.get_node_vertices g
  (*Get the list of names of only the node vertices*)
  fun get_list_node_vertex_names g = V.NSet.list_of (get_node_vertex_names g)
  (*Add a node vertex with a given name to the graph*)
  fun add_node_vertex n g = G.add_named_vertex n (mkX zero_angle) g
  (*Test to see if a vertex is a node vertex*)
  fun is_node_vertex n g = G.is_node_vertex g n
  (*Return the number of node vertices in the graph*)
  fun node_vertex_count g = V.NSet.cardinality (get_node_vertex_names g)
  
  (*********************************)
  (********Wire vertices only*******)
  (*********************************)
  
  (*Get the name set for all of the wire vertices*)
  fun get_wire_vertex_names g = G.get_wire_vertices g
  (*Get the list of names of only the wire vertices*)
  fun get_list_wire_vertex_names g = V.NSet.list_of (get_wire_vertex_names g)
  (*Add a wire vertex with a given name to the graph*)
  fun add_wire_vertex w g = G.add_named_vertex w bvert g
  (*Test to see if a vertex is a wire vertex*)
  fun is_wire_vertex n g = G.is_wire_vertex g n
  (*Return the number of wire vertices in the graph*)
  fun wire_vertex_count g = V.NSet.cardinality (get_wire_vertex_names g)
  
  (*********************************)
  (************All edges************)
  (*********************************)
  
  (*Directed and undirected types*)
  val Directed = G.Directed
  val UnDirected = G.UnDirected
  (*Return the name table for all of the edges*)
  fun get_edges g = G.get_edges g
  (*Get the name set for all of the edges*)
  fun get_edge_names g = E.NTab.get_nameset (get_edges g)
  (*Return the list of edge names in the graph*)
  fun get_list_edge_names g = E.NSet.list_of (get_edge_names g)
  (*Get the name of the source vertex given the edge name
  (Not really applicable to undirected edge)*)
  fun get_source e g = 
    let val ((_,_),(src,_)) = G.get_edge g e in src end
  (*Get the name of the target vertex given the edge name
  (Not really applicable to undirected edge)*)
  fun get_target e g = 
    let val ((_,_),(_,tgt)) = G.get_edge g e in tgt end
  (*Returns true if the edge is directed, and false otherwise*)
  fun is_directed e g =
    let
      val ((dir_or_undir,_),(_,_)) = G.get_edge g e
    in
      dir_or_undir=G.Directed
    end
  (*Add a directed edge to the graph between two vertices*)
  fun add_edge_dir src tgt g = G.doadd_edge dir_edge src tgt g
  (*Add an undirected edge to the graph between two vertices*)
  fun add_edge_undir src tgt g = G.doadd_edge undir_edge src tgt g
  (*Get the edge name of the edge between the two vertices.
  Returns NONE if no edge exists. If it is directed it will respect the direction
  passed. If it is undirected it will be returned regardless of the order
  of arguments*)
  fun get_edge src tgt g =
  let
    val adj = E.NSet.list_of (get_adj src g)
  in
    List.find (fn e => if (is_directed e g) then
        vertex_eq tgt (get_target e g)
      else
        true) adj
  end
  (*Delete the given edge from the graph*)
  fun delete_edge e g = G.delete_edge e g
  (*Return the number of edges in the graph*)
  fun edge_count g = E.NSet.cardinality (get_edge_names g)
  
  (*********************************)
  (**********All bang boxes*********)
  (*********************************)
  
  (*Get the name set for all of the bang boxes*)
  fun get_bbox_names g = G.get_bboxes g
  (*Get the list of all bang box names*)
  fun get_list_bbox_names g = B.NSet.list_of (get_bbox_names g)
  (*Return a unique bang box name not in the graph...*)
  fun unique_B_name g = B.NSet.new (get_bbox_names g) (B.mk "a")
  (*Add an empty bang box to the graph*)
  fun add_bbox b g = G.add_named_bbox b g
  (*Add a vertex to a bang box*)
  fun add_vertex_to_bbox b n g = G.doadd_to_bbox b (V.NSet.single n) g
  (*Check to see if two bang boxes are name equivalent*)
  fun bbox_eq b1 b2 = B.name_eq (b1,b2)
  (*Remove a bang box from the graph
  (Equivalent to dropping it)*)
  fun delete_bbox b g = G.drop_bbox b g
  (*Get the name of the parent of the bang box if there is one
  B.name option*)
  fun get_bbox_parent b g =
    BMap.lookup_domf (G.get_bbox_parent_rel g) b
  (*Test if there is a parent for this bang box*)
  fun has_bbox_parent b g = case (get_bbox_parent b g) of
      NONE => false
    | _ => true
  (*Set the first bang box to have the second bang box
  as its parent. The first bang box must be a subgraph of the second*)
  fun set_bbox_parent b1 b2 g = G.set_bbox_parent b1 b2 g
  (*Get rid of the parent link for this bang box (does nothing
  if the parent does not exist*)
  fun delete_bbox_parent b g = 
    if (has_bbox_parent b g) then (G.clear_bbox_parent b g) else g
  (*Get the set of all vertices in a bang box*)
  fun get_bbox_vertices b g = G.get_bbox_contents g b
  (*Return a list of all of the vertices in a bang box*)
  fun get_list_bbox_vertices b g = V.NSet.list_of (get_bbox_vertices b g)
  (*Return a list of only the node vertices in a bang box*)
  fun get_list_bbox_node_vertices b g = List.filter (fn v => is_node_vertex v g) (get_list_bbox_vertices b g)
  (*Return a list of only the wire vertices in a bang box*)
  fun get_list_bbox_wire_vertices b g = List.filter (fn v => is_wire_vertex v g) (get_list_bbox_vertices b g)
  (*Create a bang box "name" with the given string as the name*)
  fun create_bbox name = B.mk name
  (*Return the number of bang boxes in the graph*)
  fun bbox_count g = B.NSet.cardinality (get_bbox_names g)
  
  (*********************************)
  (************Printing*************)
  (*********************************)
  
  (*An alternative way to print the graph:*)
  
  (*Return a string for the node vertices*)
  fun string_node_vertices g =
    let
      fun p v = Pretty.string_of (V.pretty_name v);
      (*Print each vertex in turn:*)
      fun str [] = "]"
        | str [v] = (p v) ^ "]"
        | str (v::vs) = (p v) ^ "," ^ (str vs)
    in
      "[" ^ (str (V.NSet.list_of (G.get_node_vertices g)))
    end
  (*Return a string for the wire vertices*)
  fun string_wire_vertices g =
    let
      fun p v = Pretty.string_of (V.pretty_name v);
      (*Print each vertex in turn:*)
      fun str [] = "]"
        | str [v] = (p v) ^ "]"
        | str (v::vs) = (p v) ^ "," ^ (str vs)
    in
      "[" ^ (str (V.NSet.list_of (G.get_wire_vertices g)))
    end
  (*Return a string for the edges in the graph:*)
  fun string_edges g =
    let
      fun string_v v = Pretty.string_of (V.pretty_name v);
      fun string_dir e = if (is_directed e g) then "Dir" else "UnDir";
      fun string_node v = if (is_node_vertex v g) then "N" else "W";
      fun p e = "(" ^ (string_dir e) ^ ",(" ^
        "(" ^ (string_node (get_source e g)) ^ "," ^
        (string_v (get_source e g)) ^ ")" ^
        "," ^
        "(" ^ (string_node (get_target e g)) ^ "," ^
        (string_v (get_target e g)) ^ ")" ^
        "))"
      (*Print each edge in turn:*)
      fun str [] = "]"
        | str [e] = (p e) ^ "]"
        | str (e::es) = (p e) ^ "," ^ (str es)
    in
      "[" ^ (str (get_list_edge_names g))
    end
  (*Print the bang boxes of the graph, showing each bang box with
  its parent, and all the node vertices it contains.*)
  fun string_bboxes g =
    let
      fun p b = Pretty.string_of (B.pretty_name b);
      (*Print the parent if there is one*)
      fun string_parent b = case (get_bbox_parent b g) of
          NONE => ""
        | SOME parent => p parent;
      fun p2 v = Pretty.string_of (V.pretty_name v);
      (*Print the vertices in the bang box*)
      fun string_vs [] = "]"
        | string_vs [v] = (p2 v) ^ "]"
        | string_vs (v::vs) = (p2 v) ^ "," ^ (string_vs vs);
      fun p3 b = (p b) ^ (
        if (has_bbox_parent b g) then
          "(" ^ (string_parent b) ^ ",[" ^ (string_vs (get_list_bbox_node_vertices b g)) ^ ")"
        else
          "([" ^ (string_vs (get_list_bbox_node_vertices b g)) ^ ")");
      (*Print all of the bang boxes!*)
      fun string_bs [] = ""
        | string_bs [b] = p3 b
        | string_bs (b::bs) = (p3 b) ^ "\n" ^ (string_bs bs)
    in
      string_bs (get_list_bbox_names g)
    end
    
    (*Convert the graph to its string representation:*)
    fun graph_to_string g = "Node Vertices:\n" ^
      (string_node_vertices g) ^ "\nWire Vertices:\n" ^
      (string_wire_vertices g) ^ "\nEdges:\n" ^
      (string_edges g) ^ "\nBang boxes\n" ^
      "Name (?Parent?,[Children])\n" ^
      (string_bboxes g) ^ "\n"
    
    (*Print a graph to the terminal*)
    fun print_graph g = TextIO.print((graph_to_string g) ^ "\n")
  end;
end;
