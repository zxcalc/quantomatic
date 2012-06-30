(* Rewrite Graph: Graphs as nodes, (Rule, Match) as edges *)
(* When we do ruleset checking this is a graph (edges labelled
 * as OTHER)
 * when looking at rewrites of a given graph, this is a tree
 * and we label edges in order to avoid rewrite loops
 *)

signature RGRAPH =
sig

  structure Graph: BANG_GRAPH
  structure Rule : RULE
  structure Match: MATCH
    
  type T
  type vertex = (Graph.T * (E.NSet.T * E.NSet.T))
  (* Other when it's a graph, Tree, etct... when it's 
   * a rewrite tree *)
  datatype edge_type = Tree | Back | Forward | Cross | Other
  type edge_data = (R.name * (Rule.T * Match.T))
  type edge = ((GraphName.name * GraphName.name) * edge_data) * edge_type
  type root = GraphName.name option

  datatype status = UNVISITED | DISCOVERED | VISITED
  type discovery_time = int;
  type state = (discovery_time * status)

  val init : T

  val get_edges : T -> edge E.NTab.T
  val get_vertices : T ->  vertex GraphName.NTab.T
  val get_root : T -> root (* NONE if some connected component
                                              of the graph has not been "treeified"  *)
  val set_root : root -> T -> T
  val get_vnames : T -> GraphName.NSet.T
  val get_enames : T -> E.NSet.T

  val add_vertex : Graph.T -> T -> (GraphName.name * T)
  val add_named_vertex : Graph.T -> GraphName.name -> T -> (GraphName.name * T)
  val add_edge   : string -> ((GraphName.name * GraphName.name) * edge_data) -> T -> (E.name * T)

  val unify1 : GraphName.name -> T -> T
  val unify : T -> T

  val init_states : T -> state GraphName.NTab.T
  val set_state_status : status -> GraphName.name -> state GraphName.NTab.T ->
                                                            state GraphName.NTab.T
  val get_vertex_state_status : GraphName.name -> state GraphName.NTab.T -> status

  val treeify : GraphName.name -> T -> T

  val pretty : T -> Pretty.T
  val print : T -> unit
  
  exception no_such_vertex_exp of string * GraphName.name * T;
  exception no_such_edge_exp of string * E.name * T;
  exception no_such_vertex_state_exp of (string * GraphName.name * (state GraphName.NTab.T))

end;

functor RGraph(
  structure RSetRewrites : RULESET_REWRITES 
  ) : RGRAPH
= struct

  structure RSetRewrites = RSetRewrites;
  structure Rule = RSetRewrites.Rule;
  structure Graph = Rule.Graph;
  structure Match = RSetRewrites.Match;

  type vertex = (Graph.T * (E.NSet.T * E.NSet.T))
  datatype edge_type = Tree | Back | Forward | Cross | Other
  type edge_data = R.name * (Rule.T * Match.T)
  type edge = ((GraphName.name * GraphName.name) * edge_data) * edge_type
  type root = GraphName.name option

  datatype status = UNVISITED | DISCOVERED | VISITED
  type discovery_time = int;
  type state = (discovery_time * status)

  datatype T = RGraph of {
                        edges : edge E.NTab.T,
                        vertices : vertex GraphName.NTab.T,
                        root: root
                     };

  exception no_such_vertex_exp of string * GraphName.name * T;
  exception no_such_edge_exp of string * E.name * T;
  exception no_such_vertex_state_exp of (string * GraphName.name * (state GraphName.NTab.T))

  fun update_edges f (RGraph rg) =
    RGraph { edges = (f (#edges rg)), vertices = (#vertices rg), root = #root rg};
  fun update_vertices f (RGraph rg) = 
    RGraph { edges = (#edges rg), vertices = (f (#vertices rg)), root = #root rg };
  fun update_root f (RGraph rg) =
    RGraph { edges = #edges rg, vertices = #vertices rg, root = f (#root rg) };
  val set_edges = update_edges o K;
  val set_vertices = update_vertices o K;
  val set_root = update_root o K;

  fun get_edges (RGraph {edges = e,...}) = e; 
  fun get_vertices (RGraph {vertices = v,...}) = v; 
  fun get_root (RGraph {root = r,...}) = r;

  val get_vnames = GraphName.NTab.get_nameset o get_vertices;
  val get_enames = E.NTab.get_nameset o get_edges;

  fun get_vertex rg vn = 
    case GraphName.NTab.lookup (get_vertices rg) vn
      of SOME v => v
       | NONE => raise no_such_vertex_exp ("get_vertex", vn, rg)

  fun get_edge rg en =
    case E.NTab.lookup (get_edges rg) en
      of SOME e => e
       | NONE => raise no_such_edge_exp ("get_edge", en, rg)

  val init = RGraph { edges = E.NTab.empty, 
                      vertices = GraphName.NTab.empty,
                      root = NONE}

  fun add_named_vertex g name rg = 
    let
      val vertices = get_vertices rg
      val (nm, vertices') = GraphName.NTab.add 
              (name, (g, (E.NSet.empty, E.NSet.empty))) vertices
    in
      (nm, set_vertices vertices' rg)
    end;

  fun add_vertex g rg = add_named_vertex g GraphName.default_name rg

  fun rename_vertex nm1 nm2 rg = 
    let 
      val vertices = get_vertices rg
      val edges = get_edges rg
      val v = case GraphName.NTab.lookup vertices nm1
                of SOME v => v
                 | NONE => raise no_such_vertex_exp ("rename_vertex",nm1,rg)
      val ins = (fst o snd) v
      val outs = (snd o snd) v
      val edges' = edges 
                    |> E.NSet.fold (E.NTab.map_entry (fn (((from,_),d),t) => 
                                                       (((from,nm2),d),t))) ins
                    |> E.NSet.fold (E.NTab.map_entry (fn (((_,tos),d),t) => 
                                                       (((nm2,tos),d),t))) outs
    in
      rg  |> set_edges edges'
          |> set_vertices (snd (GraphName.NTab.rename1 nm1 nm2 vertices))
    end;

  fun add_named_edge_internal nm edge_type e rg = 
    let
      val edges = get_edges rg
      val vertices = get_vertices rg
      val (nm, edges') = E.NTab.add (nm, (e, edge_type)) edges
      val vertices' = vertices |> GraphName.NTab.map_entry (fn (n,(ins,outs)) => 
                                                    (n,(ins,E.NSet.ins_fresh nm outs))) 
                                                        ((fst o fst) e)
                               |> GraphName.NTab.map_entry (fn (n,(ins,outs)) => 
                                                    (n, (E.NSet.ins_fresh nm ins,outs))) 
                                                        ((snd o fst) e)
    in
      (nm, rg |> set_edges edges'
              |> set_vertices vertices')
    end;

  fun add_named_edge nm "tree" = add_named_edge_internal nm Tree
    | add_named_edge nm "back" = add_named_edge_internal nm Back
    | add_named_edge nm "forward" = add_named_edge_internal nm Forward
    | add_named_edge nm "cross" = add_named_edge_internal nm Cross
    | add_named_edge nm _ = add_named_edge_internal nm Other

 
  fun edge_type_to_string Tree = "tree"
    | edge_type_to_string Back = "back"
    | edge_type_to_string Forward = "forward"
    | edge_type_to_string Cross = "cross"
    | edge_type_to_string _ = "other"
  val add_edge = add_named_edge E.default_name
 
  fun delete_edge en g = 
    let 
      val edges = get_edges g
      val (((fromvn,tovn),d),t) =
          case E.NTab.lookup edges en of 
            NONE => raise no_such_edge_exp ("delete_edge", en, g)
          | SOME x => x;

      val edges2 = edges |> E.NTab.delete en

      (* delete all edges going into or out of this vertex. *)
      val vertices2 = 
          (get_vertices g)
            |> GraphName.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(ins,E.NSet.delete en outs))) fromvn
            |> GraphName.NTab.map_entry (fn (d,(ins,outs)) => 
                                         (d,(E.NSet.delete en ins,outs))) tovn;

      (* give back new edge name and new graph *)
    in g |> set_vertices vertices2
         |> set_edges edges2 
    end;

  fun delete_vertex n g = 
    let
      val vertices = get_vertices g
      val (nd,(ins,outs)) =
          case GraphName.NTab.lookup vertices n of 
            NONE => raise no_such_vertex_exp ("delete_vertex", n, g)
          | SOME x => x
      (* delete all edges going into or out of this vertex. *)
      val g2 = 
          g |> E.NSet.fold delete_edge ins
            |> E.NSet.fold delete_edge (E.NSet.subtract outs ins)

    in 
      g2 |> update_vertices (GraphName.NTab.delete n)
    end;



   fun flip f x y = f y x
   (* Functions to check if two vertices (e.g. graphs) are isomorphic, if so
    * merge the two vertices.*)
   fun are_isomorphic g1 g2 =
     let
      val r  = Rule.mk (g1,g1)
      val r' = Rule.mk (g2,g2)
      fun has_match rule g = case (Seq.pull o snd o 
                                    (flip RSetRewrites.rule_matches g)) rule
                              of SOME _ => true
                               | NONE => false
     in
      (has_match r g2) andalso (has_match r' g1)
     end;

  fun merge_vertices v1 v2 g = 
    let 
      val (_,(ines,outes)) = get_vertex g v1
      val new_name = GraphName.mk
        ("(" ^ GraphName.dest v1 ^ "M" ^ GraphName.dest v2 ^ ")")
    in 
        g |> E.NSet.fold
              (fn en => fn g => 
                  let val (((fromv,tov),d),t) = get_edge g en
                  in g |> delete_edge en
                       |> add_named_edge en (edge_type_to_string t) ((fromv,v2),d)
                       |> snd
                  end)
              ines
          |> E.NSet.fold
              (fn en => fn g => 
                  let val (((fromv,tov),d),t) = get_edge g en
                  in g |> delete_edge en
                       |> add_named_edge en (edge_type_to_string t) ((v2, tov),d)
                       |> snd
                  end)
              outes
          |> delete_vertex v1
      end;

  (* Unify1: take a graphname and see if there are any isomorphic graphs in the 
   * rewrite graph => merge_vertices  *)
  fun unify1 gn rg =
    let
      val gnames = GraphName.NSet.delete gn (GraphName.NTab.get_nameset
                                              (get_vertices rg))
      val (g,(_,_)) = case GraphName.NTab.lookup (get_vertices rg) gn
                        of SOME a => a
                         | NONE => raise no_such_vertex_exp ("unify1", gn, rg)
      fun check_and_unify n rg' = case GraphName.NTab.lookup (get_vertices rg') n
                                of SOME (v,(_,_)) => if (are_isomorphic v g)
                                                      then merge_vertices n gn rg'
                                                      else rg'
                                 | NONE => rg'
    in
      rg |> GraphName.NSet.fold (check_and_unify) gnames
    end;

  fun unify rg = 
    let
      fun check_and_unify n rg' = case GraphName.NTab.lookup (get_vertices rg') n
                                  of SOME _ => unify1 n rg'
                                   | NONE => rg'
    in
      rg |> 
          GraphName.NSet.fold (check_and_unify) 
                      (GraphName.NTab.get_nameset (get_vertices rg))
    end;

  fun clear_labels rg = set_edges (E.NTab.map_all (fn _ => (fn (a,_) => (a, Other)))
                                        (get_edges rg)) rg

  (* Given a vertex, that we set as root, we can label the edges in its
   * connected component as Tree, Cross ,etc...*)

  (* Keep track of the status of the vertices as well as their discovery times *)
  fun init_states rg = 
    let
      val vnames = GraphName.NTab.get_nameset (get_vertices rg)
    in
      GraphName.NSet.fold (fn n => (fn t => snd
        (GraphName.NTab.add (n,(0, UNVISITED)) t)))
                vnames GraphName.NTab.empty
    end;

  fun get_vertex_state_dtime vn states = 
    case GraphName.NTab.lookup states vn
      of SOME (a,_) => a
       | NONE => raise no_such_vertex_state_exp ("get_vertex_state_dtime", vn, states)
  fun get_vertex_state_status vn states = 
    case GraphName.NTab.lookup states vn
      of SOME (_,a) => a
       | NONE => raise no_such_vertex_state_exp ("get_vertex_state_status", vn, states)

  fun set_state_dtime d_time = 
    GraphName.NTab.map_entry (fn (_,b) => (d_time,b))
  fun set_state_status status = 
    GraphName.NTab.map_entry (fn (a,_) => (a,status))

  fun get_children_of v rg =
    let
      val outs = case GraphName.NTab.lookup (get_vertices rg) v
                  of SOME (_,(_,a)) => E.NSet.list_of a
                   | NONE => raise no_such_vertex_exp ("get_children_of", v,rg)
      fun lookup name = case E.NTab.lookup (get_edges rg) name
                      of SOME a => ((snd o fst o fst) a,name)
                       | NONE => raise no_such_edge_exp ("get_children_of",name,rg)
    in
      List.map lookup outs
    end;

  (* Core of the algo: recursively called. First call takes the name of the root *)

  fun visit v states time rg =
    let
      val time' = time + 1
      val states' = states |> set_state_dtime time' v
                           |> set_state_status DISCOVERED v
      fun classify un (vn,en) ((states'',time''),rg) =
        (let
          fun set_edge_type t = rg |> get_edges
                                   |> E.NTab.map_entry (fn (((a,b),c),_) =>
                                                   (((a,b),c),t)) en
                                   |> (flip set_edges) rg
        in
          case (get_vertex_state_status vn states'')
            of UNVISITED => (let  
                              val ((sts,tm),rg') = visit vn states'' time''
                                                (set_edge_type Tree)
                            in
                              ((sts |> set_state_status VISITED vn
                                , tm), rg')
                            end)
            | DISCOVERED => ((states'', time''),set_edge_type Back)
            | VISITED =>  if (get_vertex_state_dtime un states'') <
                (get_vertex_state_dtime vn states'')
                            then ((states'',time''), set_edge_type Forward)
                            else ((states'',time''), set_edge_type Cross)
        end) 
    in
      ((states',time'), rg) |> fold (classify v) (get_children_of v rg)
    end;

  fun treeify r rg =
    let
       val states = init_states rg
       val time = 0
    in
      rg |> set_root (SOME r)
         |> clear_labels 
         |> visit r states time
         |> snd
    end;

  fun pretty_edge_type Tree = Pretty.str "Tree"
    | pretty_edge_type Back = Pretty.str "Back" 
    | pretty_edge_type Forward = Pretty.str "Forward" 
    | pretty_edge_type Cross = Pretty.str "Cross" 
    | pretty_edge_type Other = Pretty.str "Other" 

  val pretty_edge_data = R.pretty_name o fst

  fun pretty g = 
    Pretty.chunks 
      [Pretty.str "Rewrite Graph{",
       Pretty.block 
         [Pretty.str "  ", (* indent *)
          Pretty.chunks 
          [Pretty.block
            [Pretty.str "Vertices: ",
              Pretty.chunks 
                (map 
                   (fn (n, (nd,(ins,outs))) => 
                       Pretty.block 
                         [GraphName.pretty_name n,
                          Pretty.str ":=",
                          Pretty.list "i{" "}" (map E.pretty_name (E.NSet.list_of ins)),
                          Pretty.list "o{" "}" (map E.pretty_name (E.NSet.list_of outs))
                   ])
                   (GraphName.NTab.list_of (get_vertices g)))],
           Pretty.block
             [Pretty.str "Edges: ",
              Pretty.chunks 
                (map 
                   (fn (en, (((v1,v2), ed), et)) => 
                       Pretty.block 
                         [E.pretty_name en,
                          Pretty.str ":= ",
                          GraphName.pretty_name v1,
                          Pretty.str " ==> ",
                          GraphName.pretty_name v2,
                          Pretty.str " ( ",
                          pretty_edge_data ed,
                          Pretty.str "  ",
                          pretty_edge_type et,
                          Pretty.str " ) "
                   ])
                   (E.NTab.list_of (get_edges g)))]]],
       Pretty.str "}"
      ];
     
val print = Pretty.writeln o pretty;

end;
