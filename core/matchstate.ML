(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  MatchState and Matching Algorithm *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(* Matching treats graphs as undirected: IMPROVE: chnage graph rep to
   be undirected and have direction as edge data, w.r.t. order of
   names of unordered pair of vertices being connected.

Simple cases: those where number of vertices between edges are matched
exactly and no chasing of boundary vertices is needed.

Simple case: 
Ba -3- Xb = Na -3- Nb

Complex case needs B-Vertex tentacle chasing: 
Ba -1- Bb -1- Bc -1- Bd = Na -3- Nb

FIXME: Consider how a semi-local strictness condition can be
included: this is needed to avoid large (symmetric) failure
space. E.g. consider pattern of two disconected graphs (although the
same issue also comes up in connected grahs) : if one fails to match,
we will keep searching for match of the other one and only fail at the
end when we realise we have extra unmatched target edges. This can be
fixed by pull-back links annotated in pattern graph. The way
pull-backs work is that you know the unmatched boundary edges and
where they can reach to add extra edges. This gives a local-matching
criteria so that we can be locally strict. Local matching can then
have some non-local effects: causes a "ver-vertex" tentacle to become
folded back. This gives efficient failure.

*)


signature MATCH_STATE
= sig
  structure Match : MATCH
  structure RGGraph : RG_GRAPH
  structure Edge : EDGE
  structure Vertex : RG_VERTEX

  sharing Vertex = RGGraph.RGVertex
  sharing Vertex = Match.MCtxt.BVertex
  sharing Edge = Match.MCtxt.Edge
  sharing Edge = RGGraph.Edge

  type T
  datatype matchstep =
           FoundMatch of T
         | Progress of T Seq.seq

  exception locally_match_2vs_exp of (Vertex.name * Vertex.name) 
                                     * Vertex.name * Vertex.name
                                     * T; 

  val init : Match.T 
             -> RGGraph.T (* pat *)
             -> RGGraph.T (* tgt *)
             -> Vertex.name Vertex.NTab.T (* initial agenda *)
             -> T (* initial match state *)

  (* some info about a match state *)
  val no_extra_needed_tgt_edges : T -> bool
  val tgt_vertex_is_unmatched : T -> Vertex.name -> bool

  (* match two graphs *)
  val match : RGGraph.T -> RGGraph.T (* pat, tgt *)
              -> Vertex.name Vertex.NTab.T (* initial agenda *)
              -> Match.T Seq.seq
  val match' : Match.T (* initial matching *)
               -> RGGraph.T -> RGGraph.T (* pat, tgt *)
               -> Vertex.name Vertex.NTab.T (* agenda *)
               -> Match.T Seq.seq

  (* parts of the matching algorithm *)
  val do_next_match_choices : T -> matchstep

  val locally_match_2vs : Vertex.name * Vertex.name -> Vertex.name ->
                          Vertex.name -> T -> T Seq.seq
  val locally_match_verticies 
      : Vertex.name * Vertex.data2 -> Vertex.name * Vertex.data2 ->
        Vertex.NSet.T -> Vertex.NSet.T ->
        T -> T Seq.seq

  val match_2vertices_with_edges : Vertex.name -> T -> Vertex.name -> T Seq.seq

  val match_vertex : Vertex.name * Vertex.data2 -> 
                     Vertex.name * Vertex.data2 ->
                     T -> T option

  val match_search : T Seq.seq -> unit -> (Match.T * Match.T Seq.seq) option


  (* modifying match state *)
  val pop_agenda : T -> ((Vertex.name * Vertex.name) * T) option

  val add1_to_agenda : Vertex.name * Vertex.name -> T -> T
  val add_needed_edges : Edge.NSet.T * Edge.NSet.T -> T -> T
  val add_needed_edges' : Edge.NSet.T -> T -> T

  val useup1_pat_vertex : Vertex.name -> T -> T
  val useup_needed_edges : Edge.NSet.T -> T -> T
  val useup_pat_edges : Edge.NSet.T -> T -> T
  val useup_tgt_edges : Edge.NSet.T -> T -> T

  (* internal get, update and set *)
  val get_adj_pat_vertices : T -> Edge.NSet.T * Edge.NSet.T -> Vertex.NSet.T
  val get_adj_tgt_vertices : T -> Edge.NSet.T * Edge.NSet.T -> Vertex.NSet.T
  val get_agenda : T -> Vertex.name Vertex.NTab.T
  val get_match : T -> Match.T
  val get_match_mctxt : T -> Match.MCtxt.T
  val get_match_vmap : T -> Vertex.name Vertex.NTab.T
  val get_needed_tgt_es : T -> Edge.NSet.T
  val get_tgt_vnseq : T -> Vertex.name Seq.seq
  val get_tgt_vs : T -> T -> Vertex.NSet.T
  val get_unm_pat : T -> RGGraph.T
  val get_unm_pat_vs : T -> Vertex.NSet.T
  val get_unm_tgt : T -> RGGraph.T
  val get_unm_tgt_es : T -> Edge.NSet.T

  val update_agenda : (Vertex.name Vertex.NTab.T ->
                       Vertex.name Vertex.NTab.T) ->
                      T -> T
  val update_match : (Match.T -> Match.T) -> T -> T
  val update_needed_tgt_es : (Edge.NSet.T -> Edge.NSet.T) -> T -> T
  val update_unm_pat : (RGGraph.T -> RGGraph.T) -> T -> T
  val update_unm_tgt : (RGGraph.T -> RGGraph.T) -> T -> T

  val set_agenda : Vertex.name Vertex.NTab.T -> T -> T
  val set_match : Match.T -> T -> T
  val set_match_mctxt : Match.MCtxt.T -> T -> T

  (* pretty printing *)
  val pretty : T -> Pretty.T
  val pretty_agenda : T -> Pretty.T
  val pretty_match_pair : Vertex.name * Vertex.name -> Pretty.T
  val pretty_next_pat_vertex : T -> Pretty.T
  val print : T -> unit

end;



(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor MatchStateFun(
  structure RGGraph : RG_GRAPH 
  structure Match : MATCH
  sharing Match.BVertex = RGGraph.RGVertex
  sharing RGGraph.Edge = Match.Edge
) : MATCH_STATE
= struct

structure RGGraph = RGGraph;
structure Vertex = RGGraph.RGVertex;
structure Edge = RGGraph.Edge;

structure Match = Match;
structure MCtxt = Match.MCtxt;

datatype T = 
         MState of 
         {(* agenda = (tgt -> patt) = vert commited to matching each other *) 
          agenda : Vertex.name Vertex.NTab.T, (* and-agenda: pat -> tgt *)
          (* global vertex and edge matching info *)
          match : Match.T, 
          (* edges in tgt that still need to be matched *)
          needed_tgt_es : Edge.NSet.T,
          (* the pat graph with vertices removed as we match them *)
          unm_pat : RGGraph.T, 
          (* tgt with matched edges removed as we match them *)
          unm_tgt : RGGraph.T 
         };

  exception locally_match_2vs_exp of (Vertex.name * Vertex.name) 
                                     * Vertex.name * Vertex.name
                                     * T; 

(* initial matching state *)
fun init match pat tgt agenda =
    MState { agenda = agenda, match = match,
             needed_tgt_es = Edge.NSet.empty,
             unm_pat = pat, unm_tgt = tgt };

(* tools *)
fun get_agenda (MState rep) = #agenda rep;
fun get_match (MState rep) = #match rep;
fun get_unm_pat (MState rep) = #unm_pat rep;
val get_unm_pat_vs = 
    Vertex.NTab.get_nameset o RGGraph.get_vertices o get_unm_pat;
fun get_unm_tgt (MState rep) = #unm_tgt rep;
fun get_tgt_vs (MState rep) = (* tgt contains all vs; only edges are removed *)
    Vertex.NTab.get_nameset o RGGraph.get_vertices o get_unm_tgt;
val get_unm_tgt_es =
    Edge.NTab.get_nameset o RGGraph.get_edges o get_unm_tgt;
fun get_needed_tgt_es (MState rep) = #needed_tgt_es rep;


(* *)
fun update_agenda f (MState rep) = 
    MState { agenda = f (#agenda rep), match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = #unm_pat rep,
             unm_tgt = #unm_tgt rep
           };
fun update_match f (MState rep) = 
    MState { agenda = #agenda rep, match = f (#match rep),
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = #unm_pat rep,
             unm_tgt = #unm_tgt rep
           };
fun update_unm_pat f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = f (#unm_pat rep),
             unm_tgt = #unm_tgt rep
           };
fun update_unm_tgt f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = #needed_tgt_es rep,
             unm_pat = #unm_pat rep,
             unm_tgt = f(#unm_tgt rep)
           };
fun update_needed_tgt_es f (MState rep) = 
    MState { agenda = #agenda rep, match = #match rep,
             needed_tgt_es = f (#needed_tgt_es rep),
             unm_pat = #unm_pat rep,
             unm_tgt = #unm_tgt rep
           };

val set_agenda = update_agenda o K;
val set_match = update_match o K;

val get_match_vmap = Match.get_vmap o get_match;
val get_match_mctxt = Match.get_mctxt o get_match;
val set_match_mctxt = update_match o Match.set_mctxt;

(* true at end of successful matching: this ensures the strictness
   condition for edge matching (only refers to strictly needed
   edges). *)
val no_extra_needed_tgt_edges = Edge.NSet.is_empty o get_needed_tgt_es;

(* *)
fun get_adj_pat_vertices st edgens =
    RGGraph.adj_vnames' (get_unm_pat st) edgens;
fun get_adj_tgt_vertices st edgens =
    RGGraph.adj_vnames' (get_unm_tgt st) edgens;

(* implicit: st n *)
val tgt_vertex_is_unmatched = 
    Match.tgt_vertex_is_unmatched o get_match;

(* implicit: tgt_edges st *)
val useup_tgt_edges = update_unm_tgt o Edge.NSet.fold RGGraph.delete_edge;
val useup_pat_edges = update_unm_pat o Edge.NSet.fold RGGraph.delete_edge;

(* unmatched but needed edges in the pattern *)
(* : Edge.NSet.T -> T -> T *)
val add_needed_edges' = update_needed_tgt_es o Edge.NSet.union_merge;
fun add_needed_edges (ins,outs) = 
    update_needed_tgt_es (Edge.NSet.union_merge ins 
                          o Edge.NSet.union_merge outs);
val useup_needed_edges = update_needed_tgt_es o Edge.NSet.remove_set;

(* implicit: v st *)
val useup1_pat_vertex = update_unm_pat o RGGraph.delete_vertex;
(* implicit: (patv*tgtv) st *)
fun add1_to_agenda (pat_n, tgt_n) st = 
    update_agenda (Vertex.NTab.ins (pat_n,tgt_n)) st;

(* add vertex-matching to agenda if consistent (consistent = not in
   agenda, or already in the agenda but matched to same thing - in
   which case do nothing) *)
fun try_add1_to_agenda (pat_n,tgt_n) st = 
    let val agenda = get_agenda st
    in case Vertex.NTab.lookup agenda pat_n of 
        NONE => SOME (set_agenda (Vertex.NTab.ins (pat_n,tgt_n) agenda) st)
      | SOME tgt_n2 => 
        (if Vertex.name_eq (tgt_n,tgt_n2) 
         then SOME st (* already in agenda *)
         else NONE (* pat-vertex can only be mapped one tgt-vertex. *))
    end;

(* pop first vertex pair from agenda *)
fun pop_agenda st = 
    (case Vertex.NTab.pull_elem (get_agenda st)
      of NONE => NONE 
       | SOME (h,agenda2) => SOME (h,set_agenda agenda2 st));

(* given an edge matching, apply it to the state, removing edges *)
fun apply_edge_matching st (ematches,m2) = 
    let 
      val matched_tgt_edges = 
          Edge.NTab.fold (Edge.NSet.add o snd) ematches Edge.NSet.empty
      val matched_pat_edges = 
          Edge.NTab.get_nameset ematches
    in
      st |> useup_pat_edges matched_pat_edges
         |> useup_tgt_edges matched_tgt_edges
         |> set_match m2
         (* we can remove the now matched edges from the needed set *)
         |> useup_needed_edges matched_tgt_edges
         (* try to add this, must be consistent with other  
            required matchings of to_pn in agenda *)
    end;
                
(* st = matching state; moretgtvns = more tgt vertices to be matched;
(ntgt,estgt) = target vertex and edges being considerd; (npat,espat) =
pattern vertex and edges being considered. *)
fun locally_match_2vs (from_pn, to_pn) from_tn to_tn st = 
    let 
      val pat_edges = RGGraph.edges_between (get_unm_pat st) from_pn to_pn;
      val tgt_edges = RGGraph.edges_between (get_unm_tgt st) from_tn to_tn;
    in
      (Match.match_edges pat_edges tgt_edges (get_match st))
        |> Seq.map (apply_edge_matching st)
        |> Seq.map_filter I o Seq.map (try_add1_to_agenda (to_pn,to_tn))
        |> (Seq.of_list o Seq.list_of) (* force eval here and now *)
    end
    handle e => 
           (writeln ("exception! : " ^ PolyML.makestring e);
           raise locally_match_2vs_exp ((from_pn,to_pn),from_tn,to_tn,st)
           );


(* IMPROVE: combine this test with the getting of adjacent vertices ? *)
fun match_self_loops from_n from_tn st = 
    let 
      val pat = get_unm_pat st
      val tgt = get_unm_tgt st
      val pat_edges = RGGraph.get_self_loops pat from_n
      val tgt_edges = RGGraph.get_self_loops tgt from_tn
    in
      (Match.match_edges pat_edges tgt_edges (get_match st))
        |> Seq.map (apply_edge_matching st)
    end;

(* pick first pattern node and then try to match it against
   every possible target node. *)
fun locally_match_verticies (from_n,from_d) (from_tn,from_td) pns tns st = 
    let 
      fun match1 pns (tns, st) = 
          (case Vertex.NSet.pull_local_bot pns
            of NONE => 
               (case from_d (* allow extra targets only if from_d is pat vertex *)
                 of Vertex.BVert => Seq.single st
                  | Vertex.IVert _ => 
                    if Vertex.NSet.is_empty tns 
                    then Seq.single st else Seq.empty)
             | SOME (to_pn,pns2) =>                
               (Seq.of_list (Vertex.NSet.pull_each tns))
                 |> Seq.maps (fn (to_tn, tns2) => 
                                 (Seq.map 
                                    (pair tns2)
                                    (locally_match_2vs 
                                       (from_n,to_pn) from_tn to_tn st)))
                 |> Seq.maps (match1 pns2)) (* recurse to match more pattern vertices *)
    in
      (* if pat self loop, check tgt has same self loops *)
      if Vertex.NSet.contains pns from_n then 
         if Vertex.NSet.contains tns from_tn then 
           (match_self_loops from_n from_tn st)
             |> (Seq.maps 
                 (fn st' => (* dont follow self loops to match other stuff *)
                     (match1 (Vertex.NSet.delete from_n pns) 
                             (Vertex.NSet.delete from_tn tns, st'))))
             |> Seq.map (useup1_pat_vertex from_n)
         else Seq.empty
       else (match1 pns (tns, st))
              |> Seq.map (useup1_pat_vertex from_n)
    end;

(* assumes patv are uninstantiated *)
(* raises Vertex.NTab...DUP exception, if n1 is already insantiated *)
(* Assumes each pat-vertex is only examined once; therefore assume not in vmap *)
fun match_vertex (n, Vertex.BVert) (tn, _) st = 
    SOME (update_match (Match.match_bvertex n tn) st)
  | match_vertex (n, Vertex.IVert d) (tn, Vertex.IVert td) st = 
    (* map must be injective for internal pat vertices *)
    if tgt_vertex_is_unmatched st tn then 
      case Match.match_vertex_idata (n,d) (tn,td) (get_match st)
       of NONE => NONE
        | SOME m2 => SOME (st |> set_match m2)
    else NONE (* some internal vertex is already matched to the target *)
  | match_vertex (_, Vertex.IVert _) (_,Vertex.BVert) m = 
    NONE; (* instantiated vertex cannot match a bvertex (half-edge) *)


(* assumes: pat_ns are all uninstantiated *)
(* IMPROVE: add quick check of num of edges to avoid complex search
   over all permutations matchings that do not add up *)
fun match_2vertices_with_edges 
      (n : Vertex.name) (* pattern vertex name *)
      (st : T) (* match state so far *)
      (tn : Vertex.name) = (* target vertex name *)
    let 
      val pat = get_unm_pat st
      val (d,edgens) = RGGraph.get_vertex pat n;
      val (td,tedgens) = RGGraph.get_vertex (get_unm_tgt st) tn;
    in case match_vertex (n,d) (tn,td) st of 
         NONE => Seq.empty (* vertex does not match *)
       | SOME st2 => (* continue matching *)
         (if RGGraph.is_boundary pat n then 
            st2 (* non-strict matching *)
          else add_needed_edges tedgens st2) (* else strict matching *)
           |> (locally_match_verticies (n,d) (tn,td) 
                                       (get_adj_pat_vertices st2 edgens)
                                       (get_adj_tgt_vertices st2 tedgens))
    end;

(* datatype for steps of matching *)
datatype matchstep =
         FoundMatch of T
       | Progress of T Seq.seq (* or choices in matching *)

(* get tgt vertex names as a seq *)
val get_tgt_vnseq = 
    Seq.of_list o Vertex.NSet.list_of 
    o Vertex.NTab.get_nameset 
    o RGGraph.get_vertices o get_unm_tgt;

(* consider the next and-point in the agenda *)
fun do_next_match_choices st = 
    (case pop_agenda st 
      of NONE => (* nothing on the prescribed agenda *)
         (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
            NONE => (* no pattern vertices left! *)
            if no_extra_needed_tgt_edges st then 
              FoundMatch st (* because we finished! *)
            else (* matching failed with extra left over target edges *)
              Progress Seq.empty
          | SOME pvn => 
            Progress (* choices over picking a new vertex in tgt *)
              (Seq.maps
                 (match_2vertices_with_edges pvn st)
                 (get_tgt_vnseq st)))
       | SOME ((n,tn),st2) => 
         Progress (* deal with first prescribed and-choice *)
           (match_2vertices_with_edges n st2 tn));

(* given an or-choice agenda of states, apply progress steps to search
tree, until we get a match, then give that back as first elem of lazy
list. We can then rest until asked for the next thing. :P *)
fun match_search or_agenda () = 
    (case Seq.pull or_agenda of 
       NONE => NONE
     | SOME (st,more_or_choices) =>
       (case do_next_match_choices st of 
          FoundMatch st2 => 
          SOME (get_match st2, Seq.make (match_search more_or_choices))
        | Progress new_or_choices => 
          match_search (Seq.append new_or_choices more_or_choices) ()));

(* 
exception match_bug_exp of Match.T * RGGraph.T * RGGraph.T ->
               Vertex.name Vertex.NTab.T -> Match.T Seq.seq
*)
(* *)
fun match' initmatching pat tgt initagenda = 
    let val init_st = (init initmatching pat tgt initagenda)
    in Seq.make (match_search (Seq.single init_st)) end;

val match = match' Match.empty;

(* pretty printing *)
fun pretty_match_pair (a,b) = 
    Pretty.block [Pretty.str "(",
                  Vertex.pretty_name a,
                  Pretty.str "->",
                  Vertex.pretty_name b,
                  Pretty.str ")"];

fun pretty_next_pat_vertex st = 
    (case (Vertex.NSet.get_local_bot (get_unm_pat_vs st)) of 
       NONE => Pretty.str "NONE"
     | SOME n => Pretty.block [Pretty.str "SOME (",
                               Vertex.pretty_name n,
                               Pretty.str ")"]);

fun pretty_agenda st = 
    Pretty.block 
      ((case Vertex.NTab.pull_elem (get_agenda st) of 
          NONE => [Pretty.str "Agenda: []"]
        | SOME ((nextfrom,nextto),rest) => 
          [Pretty.str "Agenda: (",
           Vertex.pretty_name nextfrom,
           Pretty.str "->",
           Vertex.pretty_name nextto,
           Pretty.list ") then [" "]" 
                       (map pretty_match_pair (Vertex.NTab.list_of rest))])
       @ [Pretty.str ";  needed tgt edges: ",
          Edge.NSet.pretty (get_needed_tgt_es  st)]);

fun pretty (st as MState rep) = 
    Pretty.block 
      [Pretty.str "MatchState {",
       Pretty.chunks 
         [Match.pretty (#match rep),
          pretty_agenda st,
          Pretty.block [Pretty.str "Unmatched pat: "],
          RGGraph.pretty (#unm_pat rep),
          Pretty.block [Pretty.str "Unmatched tgt: "],
          RGGraph.pretty (#unm_tgt rep)
         ],
       Pretty.str "}"];

val print = Pretty.writeln o pretty;

end; (* MatchState structure *)


structure DB_MatchState = MatchStateFun(structure RGGraph = RGGraph 
                                           and Match = Match);

structure MatchState : MATCH_STATE = DB_MatchState;
