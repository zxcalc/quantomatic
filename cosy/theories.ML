structure GHZW_TensorData : TENSOR_DATA = 
struct
  structure Graph = GHZW_Theory.Graph
  structure Tensor = IntTensor
  
  val dimension = 2
  
  fun ghz (maxi, maxj) (i,j) = if ((i=0 andalso j=0) orelse (i=maxi andalso j=maxj)) then 1 else 0
  fun w (ins,outs) (i,j) = let
    val outsum = List.foldr (op+) 0 (Tensor.decompose_index 2 outs i)
    val insum = List.foldr (op+) 0 (Tensor.decompose_index 2 ins j)
  in if ((outsum = 1 andalso insum = ins) orelse (outsum = 0 andalso insum = (ins - 1))) then 1 else 0
  end
  
  fun tensor_for_vertex_data (Graph.OVData.IVert GHZW_VertexData.GHZ) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (ghz ((Tensor.pow 2 outs)-1, (Tensor.pow 2 ins)-1))
    | tensor_for_vertex_data (Graph.OVData.IVert GHZW_VertexData.W) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (w (ins,outs))
    | tensor_for_vertex_data (Graph.OVData.IVert GHZW_VertexData.TICK) (1,1) =
        Tensor.tensor (2,1,1) [0,1,1,0]
    | tensor_for_vertex_data (Graph.OVData.IVert GHZW_VertexData.TICK) (ins,outs) =
        Tensor.tensorf (2,ins,outs) (K 0)  (* just return a zero tensor if bad dimensions on tick *)
    | tensor_for_vertex_data (Graph.OVData.IVert GHZW_VertexData.ZERO) (ins,outs) =
        Tensor.tensorf (2,ins,outs) (K 0)  (* always return 0 tensor *)
    | tensor_for_vertex_data (Graph.OVData.EVert) _ = Tensor.id dimension 1
end

(*
structure RG_TensorDataNoPhase : TENSOR_DATA =
struct
  structure Graph = RG_Theory.Graph
  structure Tensor = IntTensor
  
  val dimension = 2
  fun red (ins,outs) (i,j) =
        if (fold (curry op+) (Tensor.decompose_index 2 outs i) 0) mod 2 =
           (fold (curry op+) (Tensor.decompose_index 2 ins j) 0) mod 2 then 1 else 0
  fun green (maxi,maxj) (i,j) = if ((i=0 andalso j=0) orelse (i=maxi andalso j=maxj)) then 1 else 0
  
  fun tensor_for_vertex_data (Graph.OVData.IVert (RG_VertexData.Znd _)) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (green ((Tensor.pow 2 outs)-1, (Tensor.pow 2 ins)-1))
    | tensor_for_vertex_data (Graph.OVData.IVert (RG_VertexData.Xnd _)) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (red (ins,outs))
    | tensor_for_vertex_data (Graph.OVData.EVert) _ = Tensor.id dimension 1
end
*)

structure RGB_TensorData : TENSOR_DATA =
struct
  structure Graph = RGB_Theory.Graph
  structure Tensor = CIntTensor
  
  val dimension = 2
  
  val green_basis = (Tensor.tensor (2,0,1) [(1,0),(0,0)], Tensor.tensor (2,0,1) [(0,0),(1,0)])
  val red_basis = (Tensor.tensor (2,0,1) [(1,0),(1,0)], Tensor.tensor (2,0,1) [(0,~1),(0,1)])
  val blue_basis = (Tensor.tensor (2,0,1) [(1,0),(0,1)], Tensor.tensor (2,0,1) [(1,0),(0,~1)])
  
  fun gen (ket1,ket2) (ins, outs) = let
    val one = Tensor.id 2 0
    val bra1 = Tensor.conjugate_transpose ket1
    val bra2 = Tensor.conjugate_transpose ket2
    val term1 = Tensor.product (funpow ins (Tensor.product bra1) one)
                               (funpow outs (Tensor.product ket1) one)
    val term2 = Tensor.product (funpow ins (Tensor.product bra2) one)
                               (funpow outs (Tensor.product ket2) one)
  in Tensor.add term1 term2
  end
  
  fun tensor_for_vertex_data (Graph.OVData.IVert RGB_VertexData.Red) io = gen red_basis io
    | tensor_for_vertex_data (Graph.OVData.IVert RGB_VertexData.Green) io = gen green_basis io
    | tensor_for_vertex_data (Graph.OVData.IVert RGB_VertexData.Blue) io = gen blue_basis io
    | tensor_for_vertex_data (Graph.OVData.EVert) _ = Tensor.id 2 1
end



(* FUNCTOR APPLICATIONS *)

structure GHZW_Enum = GraphEnumerator(structure Theory = GHZW_Theory)

structure GHZW_Metric = WeightedArityMetric(
  structure Graph = GHZW_Theory.Graph
  fun weight_for_data (Graph.OVData.IVert GHZW_VertexData.ZERO) = 1
    | weight_for_data _ = 5
)

structure GHZW_DefaultSynth = Synthesize(
  structure GraphEnum = GHZW_Enum
  structure Metric = GHZW_Metric
  structure TData = GHZW_TensorData
)

structure GHZW_RSBuilder = RulesetBuilder(structure Synthesize = GHZW_DefaultSynth)
structure GHZW_Gens = DefaultGenerators(structure Graph = GHZW_Theory.Graph)
structure GHZW_Rws = DefaultRewrites(structure Rule = GHZW_Theory.Rule)

structure RGB_Enum = GraphEnumerator(structure Theory = RGB_Theory)

structure RGB_Synth = Synthesize(
  structure GraphEnum = RGB_Enum
  structure Metric = WeightedArityMetric(structure Graph = RGB_Theory.Graph; val weight_for_data = K 1)
  structure TData = RGB_TensorData
)

structure RGB_RSBuilder = RulesetBuilder(structure Synthesize = RGB_Synth)
structure RGB_Gens = DefaultGenerators(structure Graph = RGB_Theory.Graph)
structure RGB_Rws = DefaultRewrites(structure Rule = RGB_Theory.Rule)


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
  type ('graph, 'ruleset, 'synth) T = {
    name : string,
    dotfun : 'graph -> string,
    gens : ('graph * int * int) list,
    stats : 'synth -> int * int * int,
    class_list : 'synth -> (string * 'graph EqClass.T) list,
    rs_pairs : 'ruleset -> ('graph * 'graph) R.NTab.T
  }
  
  fun get_name       ({name=x,...}:('g,'r,'s) T) = x
  fun get_dotfun     ({dotfun=x,...}:('g,'r,'s) T) = x
  fun get_gens       ({gens=x,...}:('g,'r,'s) T) = x
  fun get_stats      ({stats=x,...}:('g,'r,'s) T) = x
  fun get_class_list ({class_list=x,...}:('g,'r,'s) T) = x
  fun get_rs_pairs   ({rs_pairs=x,...}:('g,'r,'s) T) = x
end

fun rule_data flhs frhs = (R.NTab.map_all (fn _ => fn r => (flhs r, frhs r) ))

(*
val rg_data = {
  name = "RG",
  dotfun = RG_OutputGraphDot.output,
  gens = RG_Gens.gen_list 4 [RG_VertexData.Znd LinratExpr.zero, RG_VertexData.Xnd LinratExpr.zero],
  synth = RG_NoPhaseSynth.synth
}
*)


