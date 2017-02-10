structure Crib =
struct

structure G = Petri_Theory.Graph
structure D = Petri_VertexData

(* start with empty graph and build it up *)
val g = G.empty


val v1 = V.mk "v1"
val v2 = V.mk "v2"
val e1 = E.mk "e1"

val g = g |> G.add_named_vertex v1 (G.OVData.WVert)

val sw_data = G.OVData.NVert D.SWITCH
val cp_data = G.OVData.NVert D.COPY
val buf0_data = G.OVData.NVert D.BUF0
val buf1_data = G.OVData.NVert D.BUF1

val g = g |> G.add_named_vertex v2 sw_data
val g = g |> G.add_named_edge e1 (G.Directed, ()) v1 v2

fun print_data g v =
  case G.get_vertex_data g v
    of G.OVData.NVert D.SWITCH => TextIO.print "got SWITCH"
     | G.OVData.NVert D.COPY => TextIO.print "got COPY"
     | G.OVData.NVert D.BUF0 => TextIO.print "got BUF0"
     | G.OVData.NVert D.BUF1 => TextIO.print "got BUF1"
     | G.OVData.WVert => TextIO.print "got wire vertex"

(* useful functions:

... for adjacent nodes:

G.succ_vnames : G.T -> V.name -> V.NSet.T
G.pred_vnames : G.T -> V.name -> V.NSet.T

... for adjancent edges:

G.get_out_edges : G.T -> V.name -> E.NSet.T
G.get_in_edges : G.T -> V.name -> E.NSet.T

For arities, use these in conjunction with

E.NSet.cardinality : E.NSet.T -> int

*)

end