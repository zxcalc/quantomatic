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
  

  fun tensor_for_vertex_data (Graph.NVert GHZW_Data.GHZ) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (ghz ((Tensor.pow 2 outs)-1, (Tensor.pow 2 ins)-1))
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.W) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (w (ins,outs))
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.TICK) (1,1) =
        Tensor.tensor (2,1,1) [0,1,1,0]
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.TICK) (ins,outs) =
        Tensor.tensorf (2,ins,outs) (K 0)  (* just return a zero tensor if bad dimensions on tick *)
    | tensor_for_vertex_data (Graph.NVert GHZW_Data.ZERO) (ins,outs) =
        Tensor.tensorf (2,ins,outs) (K 0)  (* always return 0 tensor *)
    | tensor_for_vertex_data (Graph.WVert) _ = Tensor.id dimension 1
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
  
  fun tensor_for_vertex_data (Graph.NVert (RG_Data.Znd _)) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (green ((Tensor.pow 2 outs)-1, (Tensor.pow 2 ins)-1))
    | tensor_for_vertex_data (Graph.NVert (RG_Data.Xnd _)) (ins, outs) =
        Tensor.tensorf (2,ins,outs) (red (ins,outs))
    | tensor_for_vertex_data (Graph.WVert) _ = Tensor.id dimension 1
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
  
  fun tensor_for_vertex_data (Graph.NVert RGB_Data.Red) io = gen red_basis io
    | tensor_for_vertex_data (Graph.NVert RGB_Data.Green) io = gen green_basis io
    | tensor_for_vertex_data (Graph.NVert RGB_Data.Blue) io = gen blue_basis io
    | tensor_for_vertex_data (Graph.WVert) _ = Tensor.id 2 1
end


structure RG_TensorData : TENSOR_DATA =
struct
  structure Graph = RG_Theory.Graph
  structure Tensor = CIntTensor
  
  val dimension = 2
  
  val green_basis = (Tensor.tensor (2,0,1) [(1,0),(0,0)], Tensor.tensor (2,0,1) [(0,0),(1,0)])
  val red_basis = (Tensor.tensor (2,0,1) [(1,0),(1,0)], Tensor.tensor (2,0,1) [(1,0),(~1,0)])
  
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
  
  val zero = LinratAngleExpr.zero
  
  fun tensor_for_vertex_data (Graph.NVert (RG_Data.Xnd _)) io = gen red_basis io
    | tensor_for_vertex_data (Graph.NVert (RG_Data.Znd _)) io = gen green_basis io
    | tensor_for_vertex_data (Graph.NVert RG_Data.Hnd) _ = raise ERROR "Cannot deal with hadamards"
    | tensor_for_vertex_data (Graph.WVert) _ = Tensor.id 2 1
end


(* FUNCTOR APPLICATIONS *)
structure RG_Enum = FastGraphEnum(
  structure Theory = RG_Theory
  
  structure Metric = WeightedArityMetric(
    structure Graph = RG_Theory.Graph
    fun weight_for_data (Graph.NVert (RG_Data.Xnd _)) = 6
      | weight_for_data (Graph.NVert (RG_Data.Znd _)) = 5
      | weight_for_data _ = 4
  )
  
  structure Equiv = TensorEquiv(structure TData = RG_TensorData)
)

structure RG_Spiders = SpiderRewrites(structure Theory = RG_Theory)


structure GHZW_Enum = FastGraphEnum(
  structure Theory = GHZW_Theory
  
  structure Metric = WeightedArityMetric(
    structure Graph = GHZW_Theory.Graph
    fun weight_for_data (Graph.NVert (GHZW_Data.GHZ)) = 6
      | weight_for_data (Graph.NVert (GHZW_Data.W)) = 5
      | weight_for_data _ = 4
  )
  
  structure Equiv = TensorEquiv(structure TData = GHZW_TensorData)
)

structure GHZW_Spiders = SpiderRewrites(structure Theory = GHZW_Theory)
structure GHZW_Synth = 
struct
  structure SU = SynthUtil(structure Enum = GHZW_Enum)
  open SU
  val GHZ = GHZW_Theory.Graph.NVert GHZW_Data.GHZ
  val W = GHZW_Theory.Graph.NVert GHZW_Data.W
end

