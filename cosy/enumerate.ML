signature GRAPH_ENUMERATOR =
sig
  structure Theory : GRAPHICAL_THEORY
  val fold_graphs : (Theory.Graph.T -> 'a -> 'a) -> (* folder *)
                    (Theory.Graph.T * int * int) list * (* generators + arities *)
                    int * int * int * int -> (* in, out, max verts, max pluggings *)
                    'a -> (* base case *)
                    'a
  val fold_graphs_with_rs : Theory.Ruleset.T -> (* ruleset *)
                            (Theory.Graph.T -> 'a -> 'a) -> (* folder *)
                            (Theory.Graph.T * int * int) list * (* generators + arities *)
                            int * int * int * int -> (* in, out, max verts, max pluggings *)
                            'a -> (* base case *)
                            'a
  val rule_matches_graph : Theory.Rule.T -> Theory.Graph.T -> bool
  val has_match : Theory.Ruleset.T -> R.NSet.T -> Theory.Graph.T -> bool
end

functor GraphEnumerator (
  structure Theory : GRAPHICAL_THEORY
) : GRAPH_ENUMERATOR =
struct
  structure Theory = Theory
  structure Graph = Theory.Graph
  
  (* maintain list of available pluggings *)
  structure Plugs = NameBRelFun(structure Dom=V and Cod=V)
  
  (* get the lowest plugging *)
  fun first_plug rel = 
    case V.NSet.get_first (Plugs.get_domset rel)
      of SOME a => SOME (a, the (V.NSet.get_first (Plugs.domf rel a)))
       | NONE   => NONE
  
  fun add_gen gen gr = Graph.merge_by_vertices gr (snd (Graph.rename_apart gr gen))
  fun get_plugs gr = let
    fun not_adj_to_boundary v = Graph.fold_adj (fn (_,v',_) => fn a =>
      a andalso (not (Graph.is_boundary_vertex gr v'))) gr v true
    val filt = V.NSet.filter not_adj_to_boundary
    val (ins,outs) = (Graph.get_inputs gr, Graph.get_outputs gr)
    val (ins',outs') = (filt ins, filt outs)
  in if V.NSet.is_empty ins' orelse V.NSet.is_empty outs'
     then Plugs.empty else Plugs.empty |> Plugs.add_many ins' outs'
  end
  fun delete_plugs_like gr (p,q) plugs = let (* remove all plugs that are essentially (p,q) because of commutativity *)
    val ps = Graph.pred_vnames gr (the (V.NSet.get_first (Graph.succ_vnames gr p)))
    val qs = Graph.succ_vnames gr (the (V.NSet.get_first (Graph.pred_vnames gr q)))
  in plugs |> Plugs.del_many ps qs
  end
  (* fun dft (Lf s) vs = s::vs | dft (Br (s,ch)) vs = s::(fold dft ch vs); *)
  
  (* precondition: g contains no !-boxes *)
  fun rule_matches_concrete_graph r g = let
    val lhs = Theory.Rule.get_lhs r
    val gs = [lhs, g]
    (*val names = 
      (fold Graph.add_to_vnames  gs V.NSet.empty,
       fold Graph.add_to_enames  gs E.NSet.empty,
       fold Graph.add_to_bboxes gs B.NSet.empty)*)
    val matches = Theory.MatchSearch.match lhs g
  in case Seq.pull matches of NONE=>false | _=>true
  end
  
  val rule_matches_graph = rule_matches_concrete_graph
  
  (* matching using !-graphs as a target seems to be broken, so return false by default *)
  (*fun rule_matches_graph r g = if (Graph.VtoBoxMap.is_empty o Graph.get_bboxes) g
                               then rule_matches_concrete_graph r g
                               else false*)
  
  (*fun has_match rs in_rules target_graph = let
    val redsets = RuleName.NSet.subdivide (4 * Multithreading.max_threads_value ()) in_rules
    fun search_reds redset = RuleName.NSet.exists (fn r => rule_matches_graph (Ruleset.get_rule rs r) target_graph) redset
  in Par_List.exists search_reds redsets
  end*)
  
  fun has_match rs in_rules target_graph =
    R.NSet.exists (fn r => rule_matches_graph (Theory.Ruleset.get_rule rs r) target_graph) in_rules
  
  fun fold_graphs_with_rs rs f (generators, total_ins, total_outs, max_verts, max_plugs) base = let
    (*val rule_list = RuleName.NSet.fold (cons o (Ruleset.get_rule rs)) (Ruleset.get_rules_in_tag rs (TagName.mk "r")) []*)
    val all_reds = Theory.Ruleset.get_rules_in_tag rs (TagName.mk "r")
    fun enum_plugs (gr, ins, plugs, plug_count) accum =
      if (ins < total_ins orelse
          plug_count > max_plugs orelse
          has_match rs all_reds gr) then accum (* return if impossible or already redicible *)
      else (if ins = total_ins then f gr else I)
              (case first_plug plugs
                 of SOME (p,q) =>
                      enum_plugs (gr |> Graph.do_plug p q |> Graph.minimise, (* do plug *)
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
  
  fun fold_graphs f run base = fold_graphs_with_rs Theory.Ruleset.empty f run base
end


