(* Structure which allows the user to store arbitrary data on 
 * graphs, !-boxes, edges and vertices as a hastable. To see how
 * this interacts with the undo stack for graphs, see controller_graph.ML *)

signature USER_DATA =
  sig
    type T;
    structure Graph : BANG_GRAPH;
    type user_data = string Symtab.table

    type g_ud  = user_data;
    type v_ud  = user_data V.NTab.T; (* A symtab for each Vertex *)
    type e_ud  = user_data E.NTab.T; (* A symtab for each Edge *)
    type bb_ud = user_data B.NTab.T; (* A symtab for each !-Box *)
   
    type updater = (user_data -> user_data); 

    val init: T;

    val get_g_ud  : T -> g_ud; (* Return the symtab attached to the graph *)
    val get_vs_ud : T -> v_ud;
    val get_es_ud : T -> e_ud;
    val get_bs_ud : T -> bb_ud;

    val set_v_ud  : v_ud -> T -> T;
    val set_e_ud  : e_ud -> T -> T;
    val set_b_ud  : bb_ud -> T -> T;

    val get_v_ud  : T -> V.name -> user_data option;
    val get_e_ud  : T -> E.name -> user_data option;
    val get_bb_ud : T -> B.name -> user_data option;

    val update_g_ud  : updater -> T -> T;
    val update_v_ud  : updater -> V.name -> T -> T;
    val update_e_ud  : updater -> E.name -> T -> T;
    val update_bb_ud : updater -> B.name -> T -> T;

    val rename_vertex  : V.name -> V.name -> T -> T;
    val rename_edge    : E.name -> E.name -> T -> T;
    val rename_bangbox : B.name -> B.name -> T -> T;
    (* We want to keep our data in sync with the content of the graph 
     * it is related to: each time the graph changes see
     * if vertices||edges||!-boxes were removed *)
    val update_against_graph : T -> Graph.T -> T;

    val print_user_data : T -> unit;
    val pretty_user_data : T -> Pretty.T;

    exception vname_already_exists of (V.name * T);
    exception ename_already_exists of (E.name * T);
    exception bbbox_already_exists of (B.name * T);
  end;

functor UserData(Graph:BANG_GRAPH):USER_DATA
=struct
    structure Graph = Graph;

    type user_data = string Symtab.table;

    type g_ud  = user_data;
    type v_ud  = user_data V.NTab.T; (* A symtab for each Vertex *)
    type e_ud  = user_data E.NTab.T; (* A symtab for each Edge *)
    type bb_ud = user_data B.NTab.T; (* A symtab for each !-Box *)

    type updater = (user_data -> user_data); 
 
    datatype T = UD of {
      graph_ud : g_ud,  (* A symtab for the  Graph   *)
      vertex_ud : v_ud, (* A symtab for each Vertex *)
      edge_ud   : e_ud, (* ''                Edge   *)
      bangb_ud : bb_ud  (* ''                !-Box  *)
    }

    exception vname_already_exists of (V.name * T);
    exception ename_already_exists of (E.name * T);
    exception bbbox_already_exists of (B.name * T);

   
    (* That's the one advertised in the signature *)
    fun update_g_ud f (UD {graph_ud=g, vertex_ud=v, edge_ud=e, bangb_ud= b}) =
      UD {graph_ud =(f g), vertex_ud=v, edge_ud=e, bangb_ud=b} 
    (* Those are used internally *)
    fun update_vs_ud f (UD {graph_ud=g, vertex_ud=v, edge_ud=e, bangb_ud= b}) =
      UD {graph_ud = g, vertex_ud=(f v), edge_ud=e, bangb_ud=b} 
    fun update_es_ud f (UD {graph_ud=g, vertex_ud=v, edge_ud=e, bangb_ud= b}) =
      UD {graph_ud= g, vertex_ud=v, edge_ud=(f e), bangb_ud=b} 
    fun update_bbs_ud f (UD {graph_ud=g, vertex_ud=v, edge_ud=e, bangb_ud= b}) =
      UD {graph_ud =g, vertex_ud=v, edge_ud=e, bangb_ud=(f b)} 

    val (set_g_ud, set_v_ud, set_e_ud, set_b_ud) =
      (update_g_ud o K, update_vs_ud o K, update_es_ud o K, update_bbs_ud o K)

    (* As declared in the signature *)
    fun get_g_ud (UD {graph_ud=g,...}) = g 
    (* Those are just handy, and used internally *)
    fun get_vs_ud (UD {vertex_ud=v,...}) = v 
    fun get_es_ud (UD {edge_ud=e,...}) = e 
    fun get_bs_ud (UD {bangb_ud=b,...}) = b

    val init = UD {
      graph_ud = Symtab.empty,
      vertex_ud = V.NTab.empty,
      edge_ud = E.NTab.empty,
      bangb_ud = B.NTab.empty
    }

    (* Return the symtab corresponding to that name *)
    (* The vertex||edge||!-box should exist because
     * a preliminary check is done  *)
    fun get_v_ud ud n = 
      let
        val vs_ud = get_vs_ud ud
      in
        V.NTab.lookup vs_ud n
      end;
  
    fun get_v_ud_safe ud n =
      case get_v_ud ud n
        of SOME t => t
         | NONE => Symtab.empty

    fun get_e_ud ud n = 
      let
        val es_ud = get_es_ud ud
      in
        E.NTab.lookup es_ud n
      end;

    fun get_e_ud_safe ud n =
      case get_e_ud ud n
        of SOME t => t
         | NONE => Symtab.empty

    fun get_bb_ud ud n = 
      let
        val bs_ud = get_bs_ud ud
      in
        B.NTab.lookup bs_ud n
      end;

    fun get_bb_ud_safe ud n =
      case get_bb_ud ud n
        of SOME t => t
         | NONE => Symtab.empty

    (* Lazily attach the user_data *) 
    fun update_v_ud upd n ud = 
      let
        val vs_ud = get_vs_ud ud
        val tab = the_default (Symtab.empty) (V.NTab.lookup vs_ud n) 
        val vud2 = V.NTab.update (n, upd tab) vs_ud
      in
        set_v_ud vud2 ud
      end;
    fun update_e_ud upd n ud = 
      let
        val es_ud = get_es_ud ud
        val tab = the_default (Symtab.empty) (E.NTab.lookup es_ud n) 
        val eud2 = E.NTab.update (n, upd tab) es_ud
      in
        set_e_ud eud2 ud
      end;
    fun update_bb_ud upd n ud = 
      let
        val bs_ud = get_bs_ud ud
        val tab = the_default (Symtab.empty) (B.NTab.lookup bs_ud n) 
        val bud2 = B.NTab.update (n, upd tab) bs_ud
      in
        set_b_ud bud2 ud
      end;
      (* Call it whenever the graph changes. We *remove* elements
       * only since it is pointless to add emtpy tables anyway*)
      fun update_against_graph ud g =
        let 
          (* Check which vertices where removed*)
          val g_vnset = Graph.get_vnames g
          val ud_vnset = V.NTab.get_nameset (get_vs_ud ud)
          val v_diff = V.NSet.remove_set g_vnset ud_vnset
          val v_ud' = V.NSet.fold (V.NTab.delete) v_diff (get_vs_ud ud)
          (* Check which edges where removed *)
          val g_enset = Graph.get_enames g
          val ud_enset = E.NTab.get_nameset (get_es_ud ud)
          val e_diff = E.NSet.remove_set g_enset ud_enset
          val e_ud' = E.NSet.fold (E.NTab.delete) e_diff (get_es_ud ud)
          (* Check which bangboxes where removed *)
          val g_bnset = Graph.get_bboxes g
          val ud_bnset = B.NTab.get_nameset (get_bs_ud ud)
          val b_diff = B.NSet.remove_set g_bnset ud_bnset
          val b_ud' = B.NSet.fold (B.NTab.delete) b_diff (get_bs_ud ud)


          (* We also check whether the symtabs attached to vertices, etc...
           * is empty: no need to store an empty tab that will eventually
           * pollute the XML  *)
          (*TODO: Do all that with a functor, instead of duplucating
           * functions *)
         (*Vertices...*)
         fun clean_vertex n ud' = case V.NTab.lookup ud' n of
                                    SOME tab => if Symtab.is_empty tab
                                      then V.NTab.delete n ud'
                                      else ud'
                                    | NONE => ud'
         val vnames = V.NTab.get_nameset v_ud'
         val v_ud'' = (V.NSet.fold (clean_vertex) vnames) v_ud'
         (*Edge...*)
         fun clean_edge n ud' = case E.NTab.lookup ud' n of
                                    SOME tab => if Symtab.is_empty tab
                                      then E.NTab.delete n ud'
                                      else ud'
                                    | NONE => ud'
         val enames = E.NTab.get_nameset e_ud'
         val e_ud'' = (E.NSet.fold (clean_edge) enames) e_ud'
          (*!-Boxes...*)
         fun clean_bb n ud' = case B.NTab.lookup ud' n of
                                    SOME tab => if Symtab.is_empty tab
                                      then B.NTab.delete n ud'
                                      else ud'
                                    | NONE => ud'
         val bbnames = B.NTab.get_nameset b_ud'
         val b_ud'' = (B.NSet.fold (clean_bb) bbnames) b_ud'
 
        in
          ud
             |> set_v_ud v_ud''
             |> set_e_ud e_ud''
             |> set_b_ud b_ud''
        end;

  fun rename_vertex oldname newname ud =
    case (V.NTab.try_rename1 oldname newname (get_vs_ud ud)) of
      SOME a => set_v_ud a ud
      | NONE => raise vname_already_exists (newname, ud)
  fun rename_edge oldname newname ud =
    case (E.NTab.try_rename1 oldname newname (get_es_ud ud)) of
      SOME a => set_e_ud a ud
      | NONE => raise ename_already_exists (newname, ud)
  fun rename_bangbox oldname newname ud =
    case (B.NTab.try_rename1 oldname newname (get_bs_ud ud)) of
      SOME a => set_b_ud a ud
      | NONE => raise bbbox_already_exists (newname, ud)

  fun pretty_symtab tab = 
    let
      fun pretty_kv (k,v) = Pretty.str (" " ^ k ^ "=> " ^ v ^ ", ")
    in
      Pretty.chunks (map pretty_kv (Symtab.dest tab))
    end;
  fun pretty_nametab pretty_printer = pretty_printer pretty_symtab

  fun pretty_user_data ud = 
    Pretty.chunks
      [Pretty.str "User Data{",
       Pretty.block
        [Pretty.str " ",
         Pretty.chunks
         [Pretty.block
          [Pretty.str "Graph User Data: ",
           pretty_symtab (get_g_ud ud)],
          Pretty.block
          [Pretty.str "Vertices User Data: ",
           pretty_nametab V.NTab.pretty (get_vs_ud ud)],
           Pretty.block
          [Pretty.str "Edges User Data: ",
           pretty_nametab E.NTab.pretty (get_es_ud ud)],
            Pretty.block
          [Pretty.str "!-Boxes User Data: ",
          pretty_nametab B.NTab.pretty (get_bs_ud ud)]]],
       Pretty.str "}"]
          
  val print_user_data = Pretty.writeln o pretty_user_data;
end;
