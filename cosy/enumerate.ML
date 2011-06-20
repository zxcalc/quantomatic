signature GRAPH_ENUMERATOR =
sig
  structure Graph : BANG_GRAPH
  structure RulesetRewrites : RULESET_REWRITES
  sharing RulesetRewrites.Rule.Graph.SharingBangGraph = Graph.SharingBangGraph
  val fold_graphs : (Graph.T -> 'a -> 'a) -> (* folder *)
                    (Graph.T * int * int) list * (* generators + arities *)
                    int * int * int * int -> (* in, out, max verts, max pluggings *)
                    'a -> (* base case *)
                    'a
  val fold_graphs_with_rs : RulesetRewrites.Ruleset.T -> (* ruleset *)
                            (Graph.T -> 'a -> 'a) -> (* folder *)
                            (Graph.T * int * int) list * (* generators + arities *)
                            int * int * int * int -> (* in, out, max verts, max pluggings *)
                            'a -> (* base case *)
                            'a
end

functor GraphEnumerator (
  structure Graph : BANG_GRAPH
  structure RulesetRewrites : RULESET_REWRITES
  sharing RulesetRewrites.Rule.Graph.SharingBangGraph = Graph.SharingBangGraph
) : GRAPH_ENUMERATOR =
struct
  structure Graph = Graph
  structure RulesetRewrites = RulesetRewrites
  structure Ruleset = RulesetRewrites.Ruleset
  (* maintain list of available pluggings *)
  structure Plugs = NameBRelFun(structure Dom=Graph.Vertex and Cod=Graph.Vertex)
  
  (* get the lowest plugging *)
  fun first_plug rel = 
    case Graph.Vertex.NSet.get_first (Plugs.get_domset rel)
      of SOME a => SOME (a, the (Graph.Vertex.NSet.get_first (Plugs.domf rel a)))
       | NONE   => NONE
  
  fun add_gen gen gr = Graph.merge_by_vertices gr (snd (Graph.rename_apart gr gen))
  fun do_plug (p,q) gr = (snd o snd) (Graph.plug_by_boundary p q gr)
  fun get_plugs gr = let
    val filt = Graph.Vertex.NSet.filter (Graph.Vertex.NSet.is_empty o (Graph.get_adj_boundary gr))
    val (ins,outs) = Graph.get_in_and_out_vnames gr
    val (ins',outs') = (filt ins, filt outs)
  in if Graph.Vertex.NSet.is_empty ins' orelse Graph.Vertex.NSet.is_empty outs'
     then Plugs.empty else Plugs.empty |> Plugs.add_many ins' outs'
  end
  fun delete_plugs_like gr (p,q) plugs = let (* remove all plugs that are essentially (p,q) because of commutativity *)
    val ps = Graph.pred_vnames gr (the (Graph.Vertex.NSet.get_first (Graph.succ_vnames gr p)))
    val qs = Graph.succ_vnames gr (the (Graph.Vertex.NSet.get_first (Graph.pred_vnames gr q)))
  in plugs |> Plugs.del_many ps qs
  end
  (* fun dft (Lf s) vs = s::vs | dft (Br (s,ch)) vs = s::(fold dft ch vs); *)
  
  fun fold_graphs_with_rs rs f (generators, total_ins, total_outs, max_verts, max_plugs) base = let
    fun graph_is_reducible gr = case RulesetRewrites.apply_first rs gr of NONE=>false | _=>true
    fun enum_plugs (gr, ins, plugs, plug_count) accum =
      if (ins < total_ins orelse
          plug_count > max_plugs orelse
          graph_is_reducible gr) then accum (* return if impossible or not an irredex *)
      else (if ins = total_ins then f gr else I)
              (case first_plug plugs
                 of SOME (p,q) =>
                      enum_plugs (gr |> do_plug (p,q), (* do plug *)
                                  ins-1,
                                  plugs |> Plugs.del_dom p |> Plugs.del_cod q,
                                  plug_count+1)
                      (enum_plugs (gr,                 (* skip to next plug, remove any similar plugs *)
                                   ins,
                                   plugs |> delete_plugs_like gr (p,q),
                                   plug_count) accum)
                  | NONE => accum)
    
    fun enum_gens (gr, gens as ((g,gin,gout)::gens'), ins, outs, verts) accum =
          if (verts > max_verts orelse
              ins > total_ins + max_plugs orelse
              outs > total_outs + max_plugs) then accum                       (* return if impossible *)
          else (enum_gens (add_gen g gr, gens, ins+gin, outs+gout, verts+1)   (* add gen *)
                (enum_gens (gr, gens', ins, outs, verts) accum))              (* pop and don't add gen *)
      | enum_gens (gr, [], ins, outs, _) accum =                              (* ready for plugging *)
          if ins >= total_ins andalso ins - total_ins = outs - total_outs     (* plugging is possible *)
             then enum_plugs (gr, ins, get_plugs gr, 0) accum                 (* plug until ins = total_ins *)
             else accum
  in enum_gens (Graph.empty, generators, 0, 0, 0) base
  end
  
  fun fold_graphs f run base = fold_graphs_with_rs Ruleset.empty f run base
end


