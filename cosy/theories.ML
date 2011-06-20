structure GHZW_TensorData : TENSOR_DATA = 
struct
  structure Graph = GHZW_Theory.Graph
  
  val dimension = 2
  
  fun ghz (maxi, maxj) (i,j) = if ((i=0 andalso j=0) orelse (i=maxi andalso j=maxj)) then 1 else 0
  fun w (ins,outs) (i,j) = let
    val outsum = List.foldr (op+) 0 (Tensor.decompose_index 2 outs i)
    val insum = List.foldr (op+) 0 (Tensor.decompose_index 2 ins j)
  in if ((outsum = 1 andalso insum = ins) orelse (outsum = 0 andalso insum = (ins - 1))) then 1 else 0
  end;
  
  fun tensor_for_vertex_data (Graph.OVertex.IVert GHZW_VertexData.GHZ) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (ghz ((Tensor.pow 2 outs)-1, (Tensor.pow 2 ins)-1))
    | tensor_for_vertex_data (Graph.OVertex.IVert GHZW_VertexData.W) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (w (ins,outs))
    | tensor_for_vertex_data (Graph.OVertex.EVert) _ = Tensor.id dimension 1
end

structure RG_TensorDataNoPhase : TENSOR_DATA =
struct
  structure Graph = RG_Theory.Graph
  
  val dimension = 2
  fun red (ins,outs) (i,j) =
        if (fold (curry op+) (Tensor.decompose_index 2 outs i) 0) mod 2 =
           (fold (curry op+) (Tensor.decompose_index 2 ins j) 0) mod 2 then 1 else 0
  fun green (maxi,maxj) (i,j) = if ((i=0 andalso j=0) orelse (i=maxi andalso j=maxj)) then 1 else 0
  
  fun tensor_for_vertex_data (Graph.OVertex.IVert (RG_VertexData.Znd _)) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (green ((Tensor.pow 2 outs)-1, (Tensor.pow 2 ins)-1))
    | tensor_for_vertex_data (Graph.OVertex.IVert (RG_VertexData.Xnd _)) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (red (ins,outs))
    | tensor_for_vertex_data (Graph.OVertex.EVert) _ = Tensor.id dimension 1
end


(* FUNCTOR APPLICATIONS *)

structure GHZW_DefaultSynth = Synthesize(
  structure GraphEnum = GraphEnumerator(
    structure Graph = GHZW_Theory.Graph
    structure RulesetRewrites = GHZW_Theory.RulesetRewrites)
  structure Metric = EdgeComplexityMetric(structure Graph = GHZW_Theory.Graph)
  structure TData = GHZW_TensorData
)

structure GHZW_Gens = DefaultGenerators(structure Graph = GHZW_Theory.Graph)
structure GHZW_Rws = DefaultRewrites(structure Rule = GHZW_Theory.Rule)

structure GHZW_RSBuilder = RulesetBuilder(
  structure Synthesize = GHZW_DefaultSynth
  structure Thy = GHZW_Theory
)

(*
structure RG_NoPhaseSynth = Synthesize(
  structure GraphEnum = GraphEnumerator(structure Graph = RG_Theory.Graph)
  structure Metric = EdgeComplexityMetric(structure Graph = RG_Theory.Graph)
  structure TData = RG_TensorDataNoPhase
)

structure RG_Gens = DefaultGenerators(structure Graph = RG_Theory.Graph)
*)



structure TheoryData =
struct
  type ('g, 'r) T = {
    name : string,
    dotfun : 'g -> string,
    gens : ('g * int * int) list,
    stats : (int * int * int * int) * ('g EqClass.T) Tensor.Tab.table -> int * int * int,
    rs_pairs : 'r -> ('g * 'g) RuleName.NTab.T
  }
  
  fun get_name     ({name=x,...}:('g,'r) T) = x
  fun get_dotfun   ({dotfun=x,...}:('g,'r) T) = x
  fun get_gens     ({gens=x,...}:('g,'r) T) = x
  fun get_stats    ({stats=x,...}:('g,'r) T) = x
  fun get_rs_pairs ({rs_pairs=x,...}:('g,'r) T) = x
end

fun rule_data flhs frhs = (RuleName.NTab.map_all (fn _ => fn r => (flhs r, frhs r) ))

(*
val rg_data = {
  name = "RG",
  dotfun = RG_OutputGraphDot.output,
  gens = RG_Gens.gen_list 4 [RG_VertexData.Znd LinratExpr.zero, RG_VertexData.Xnd LinratExpr.zero],
  synth = RG_NoPhaseSynth.synth
}
*)


