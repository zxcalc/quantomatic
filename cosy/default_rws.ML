functor DefaultRewrites (
  structure Rule : RULE
) =
struct
  structure Rule = Rule
  structure Graph = Rule.Graph
  
  val edge = (Graph.Directed, Graph.Edge.default_data)

  fun bx g = let
    val (vn,g) = g |> Graph.add_vertex Graph.OVertex.EVert
    val (bn,g) = g |> Graph.new_box Graph.BBox.default_name
  in (vn, Graph.move_to_box bn (Graph.Vertex.NSet.single vn) g)
  end

  fun frob data = let
    val rhs = Graph.empty
    val (b1,rhs) = rhs |> bx
    val (b2,rhs) = rhs |> bx
    val (b3,rhs) = rhs |> bx
    val (b4,rhs) = rhs |> bx
    val (i1,rhs) = rhs |> Graph.add_vertex (Graph.OVertex.IVert data)
    val rhs = rhs |> Graph.doadd_edge edge b1 i1 |> Graph.doadd_edge edge i1 b2
    val (i2,lhs) = rhs |> Graph.add_vertex (Graph.OVertex.IVert data)
    val lhs = lhs |> Graph.doadd_edge edge b3 i2 |> Graph.doadd_edge edge i2 b4 |> Graph.doadd_edge edge i1 i2
    val rhs = rhs |> Graph.doadd_edge edge b3 i1 |> Graph.doadd_edge edge i1 b4
  in Rule.mk (lhs,rhs)
  end
  
  fun special data = let
    val rhs = Graph.empty
    val (b1,rhs) = rhs |> bx
    val (b2,rhs) = rhs |> bx
    val (i1,rhs) = rhs |> Graph.add_vertex (Graph.OVertex.IVert data)
    val rhs = rhs |> Graph.doadd_edge edge b1 i1 |> Graph.doadd_edge edge i1 b2
    val lhs = rhs |> Graph.doadd_edge edge i1 i1
  in Rule.mk (lhs,rhs)
  end

end