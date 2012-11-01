functor SpiderRewrites (
  structure Theory : GRAPHICAL_THEORY
) =
struct
  structure Ruleset = Theory.Ruleset
  structure Rule = Theory.Rule
  structure Graph = Theory.Graph
  
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
    val (i1,rhs) = rhs |> Graph.add_vertex (Graph.OVData.NVert data)
    val rhs = rhs |> Graph.doadd_edge edge b1 i1 |> Graph.doadd_edge edge i1 b2
    val (i2,lhs) = rhs |> Graph.add_vertex (Graph.OVData.NVert data)
    val lhs = lhs |> Graph.doadd_edge edge b3 i2 |> Graph.doadd_edge edge i2 b4 |> Graph.doadd_edge edge i1 i2
    val rhs = rhs |> Graph.doadd_edge edge b3 i1 |> Graph.doadd_edge edge i1 b4
  in Rule.mk (lhs,rhs)
  end
  
  fun special data = let
    val rhs = Graph.empty
    val (b1,rhs) = rhs |> bx
    val (b2,rhs) = rhs |> bx
    val (i1,rhs) = rhs |> Graph.add_vertex (Graph.OVData.NVert data)
    val rhs = rhs |> Graph.doadd_edge edge b1 i1 |> Graph.doadd_edge edge i1 b2
    val lhs = rhs |> Graph.doadd_edge edge i1 i1
  in Rule.mk (lhs,rhs)
  end

  val t_redex = TagName.mk "redex"

  fun add_rules d rs =
  let
    val (fr, rs) = Ruleset.add_fresh_rule (R.mk "fr_0", frob d) rs
    val (sp, rs) = Ruleset.add_fresh_rule (R.mk "sp_0", special d) rs
  in rs |> Ruleset.tag_rule fr t_redex
        |> Ruleset.tag_rule sp t_redex
  end

  fun ruleset_from_vdata ds = fold add_rules ds Ruleset.empty
end
