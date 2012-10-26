(* pre-add spider laws *)
functor SpiderEqClasses (
  structure Theory : GRAPHICAL_THEORY
  structure EqClassTab : EQ_CLASS_TAB
  sharing Theory.Graph.SharingOGraph =
          EqClassTab.GraphEntry.Graph.SharingOGraph
  val default_data : EqClassTab.GraphEntry.data
  val default_edata : EqClassTab.GraphEntry.Equiv.T
) =
struct
  structure Graph = Theory.Graph
  structure GraphEntry = EqClassTab.GraphEntry
  
  val edge = (Graph.Directed, Graph.EData.default_data)

  fun bx g = let
    val (vn,g) = g |> Graph.add_vertex Graph.OVData.WVert
    val (bn,g) = g |> Graph.add_bbox
  in (vn, #2 (Graph.add_to_bbox bn (V.NSet.single vn) g))
  end

  fun frob data = let
    val rhs = Graph.empty
    val (b1,rhs) = rhs |> bx
    val (b2,rhs) = rhs |> bx
    val (b3,rhs) = rhs |> bx
    val (b4,rhs) = rhs |> bx
    val (i1,rhs) = rhs |> Graph.add_vertex (data)
    val rhs = rhs |> Graph.doadd_edge edge b1 i1 |> Graph.doadd_edge edge i1 b2
    val (i2,lhs) = rhs |> Graph.add_vertex (data)
    val lhs = lhs |> Graph.doadd_edge edge b3 i2 |> Graph.doadd_edge edge i2 b4 |> Graph.doadd_edge edge i1 i2
    val rhs = rhs |> Graph.doadd_edge edge b3 i1 |> Graph.doadd_edge edge i1 b4
  in (GraphEntry.mk' ((rhs, default_edata), default_data),
     [GraphEntry.mk' ((lhs, default_edata), default_data)])
  end
  
  fun special data = let
    val rhs = Graph.empty
    val (b1,rhs) = rhs |> bx
    val (b2,rhs) = rhs |> bx
    val (i1,rhs) = rhs |> Graph.add_vertex (data)
    val rhs = rhs |> Graph.doadd_edge edge b1 i1 |> Graph.doadd_edge edge i1 b2
    val lhs = rhs |> Graph.doadd_edge edge i1 i1
  in (GraphEntry.mk' ((rhs, default_edata), default_data),
     [GraphEntry.mk' ((lhs, default_edata), default_data)])
  end
  
  fun eq_class_tab data_list =
    fold (EqClassTab.add_eq_class o frob) data_list EqClassTab.empty

end