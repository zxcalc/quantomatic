(**
 * Annotations on a graph
 *
 * Assumes all components have the same annotation type.
 *)
signature GRAPH_ANNOTATIONS =
sig
  (* The annotation structure *)
  type T;
  (* The type of the annotations *)
  type data;

  (* With no annotations *)
  val init: T;

  val empty_data : data;

  val get_graph_annotation : T -> data;
  val get_vertex_annotation : T -> V.name -> data;
  val get_edge_annotation : T -> E.name -> data;
  val get_bangbox_annotation : T -> B.name -> data;

  val get_vertex_annotation_tab : T -> data V.NTab.T;
  val get_edge_annotation_tab : T -> data E.NTab.T;
  val get_bangbox_annotation_tab : T -> data B.NTab.T;

  val set_graph_annotation : data -> T -> T;
  val set_vertex_annotation : V.name -> data -> T -> T;
  val set_edge_annotation : E.name -> data -> T -> T;
  val set_bangbox_annotation : B.name -> data -> T -> T;

  val update_graph_annotation : (data -> data) -> T -> T;
  val update_vertex_annotation : V.name -> (data -> data) -> T -> T;
  val update_edge_annotation : E.name -> (data -> data) -> T -> T;
  val update_bangbox_annotation : B.name -> (data -> data) -> T -> T;

  val remove_vertex_annotation : V.name -> T -> T;
  val remove_edge_annotation : E.name -> T -> T;
  val remove_bangbox_annotation : B.name -> T -> T;

  val rename_vertex : V.name -> V.name -> T -> T;
  val rename_edge : E.name -> E.name -> T -> T;
  val rename_bangbox : B.name -> B.name -> T -> T;

  (* Arg1 is components to retain *)
  val cleanup : (V.NSet.T * E.NSet.T * B.NSet.T) -> T -> T;
  val apply_renaming : (V.Rnm.T * E.Rnm.T * B.Rnm.T) -> T -> T;

  val pretty : T -> Pretty.T;
  val print : T -> unit;
  val pretty_data : data -> Pretty.T;
  val print_data : data -> unit;
end;

functor GraphAnnotations(
  type data
  val data_init : data
  val data_pretty : data -> Pretty.T
) : GRAPH_ANNOTATIONS =
struct
  type data = data;
  val empty_data = data_init;

  datatype T = GraphAnn of
    {
      graph_annotation    : data,
      vertex_annotation_tab  : data V.NTab.T,
      edge_annotation_tab    : data E.NTab.T,
      bangbox_annotation_tab : data B.NTab.T
    };

  val init = GraphAnn
    {
      graph_annotation    = data_init,
      vertex_annotation_tab  = V.NTab.empty,
      edge_annotation_tab    = E.NTab.empty,
      bangbox_annotation_tab = B.NTab.empty
    };

  fun update_graph_annotation f (GraphAnn r) = GraphAnn {
    graph_annotation    = f(#graph_annotation r),
    vertex_annotation_tab  = #vertex_annotation_tab r,
    edge_annotation_tab    = #edge_annotation_tab r,
    bangbox_annotation_tab = #bangbox_annotation_tab r
  }

  fun update_vertex_annotation_tab f (GraphAnn r) = GraphAnn {
    graph_annotation    = #graph_annotation r,
    vertex_annotation_tab  = f(#vertex_annotation_tab r),
    edge_annotation_tab    = #edge_annotation_tab r,
    bangbox_annotation_tab = #bangbox_annotation_tab r
  }

  fun update_edge_annotation_tab f (GraphAnn r) = GraphAnn {
    graph_annotation    = #graph_annotation r,
    vertex_annotation_tab  = #vertex_annotation_tab r,
    edge_annotation_tab    = f(#edge_annotation_tab r),
    bangbox_annotation_tab = #bangbox_annotation_tab r
  }

  fun update_bangbox_annotation_tab f (GraphAnn r) = GraphAnn {
    graph_annotation    = #graph_annotation r,
    vertex_annotation_tab  = #vertex_annotation_tab r,
    edge_annotation_tab    = #edge_annotation_tab r,
    bangbox_annotation_tab = f(#bangbox_annotation_tab r)
  }

  fun get_graph_annotation    (GraphAnn r) = #graph_annotation r
  fun get_vertex_annotation_tab  (GraphAnn r) = #vertex_annotation_tab r
  fun get_edge_annotation_tab    (GraphAnn r) = #edge_annotation_tab r
  fun get_bangbox_annotation_tab (GraphAnn r) = #bangbox_annotation_tab r

  fun set_graph_annotation    x  = update_graph_annotation    (fn _ => x)

  fun get_vertex_annotation ann v =
    case V.NTab.lookup (get_vertex_annotation_tab ann) v
      of SOME x => x
       | NONE => data_init;

  fun get_edge_annotation ann e =
    case E.NTab.lookup (get_edge_annotation_tab ann) e
      of SOME x => x
       | NONE => data_init;

  fun get_bangbox_annotation ann e =
    case B.NTab.lookup (get_bangbox_annotation_tab ann) e
      of SOME x => x
       | NONE => data_init;

  fun set_vertex_annotation v d =
    update_vertex_annotation_tab (V.NTab.update (v,d));
  fun set_edge_annotation e d =
    update_edge_annotation_tab (E.NTab.update (e,d));
  fun set_bangbox_annotation b d =
    update_bangbox_annotation_tab (B.NTab.update (b,d));

  fun update_vertex_annotation v f ann =
    update_vertex_annotation_tab (V.NTab.map_entry f v) ann
    handle V.NTab.Tab.UNDEF _ => set_vertex_annotation v (f data_init) ann;
  fun update_edge_annotation e f ann =
    update_edge_annotation_tab (E.NTab.map_entry f e) ann
    handle E.NTab.Tab.UNDEF _ => set_edge_annotation e (f data_init) ann;
  fun update_bangbox_annotation b f ann =
    update_bangbox_annotation_tab (B.NTab.map_entry f b) ann
    handle B.NTab.Tab.UNDEF _ => set_bangbox_annotation b (f data_init) ann;

  val remove_vertex_annotation = update_vertex_annotation_tab o V.NTab.delete;
  val remove_edge_annotation = update_edge_annotation_tab o E.NTab.delete;
  val remove_bangbox_annotation = update_bangbox_annotation_tab o B.NTab.delete;

  fun rename_vertex old new = let
    fun force_rnm tab =
          case V.NTab.try_rename1 old new tab
            of SOME tab' => tab'
             | NONE => the (V.NTab.try_rename1 old new (V.NTab.delete new tab));
  in
    update_vertex_annotation_tab force_rnm
  end;
  fun rename_edge old new = let
    fun force_rnm tab =
          case E.NTab.try_rename1 old new tab
            of SOME tab' => tab'
             | NONE => the (E.NTab.try_rename1 old new (E.NTab.delete new tab));
  in
    update_edge_annotation_tab force_rnm
  end;
  fun rename_bangbox old new = let
    fun force_rnm tab =
          case B.NTab.try_rename1 old new tab
            of SOME tab' => tab'
             | NONE => the (B.NTab.try_rename1 old new (B.NTab.delete new tab));
  in
    update_bangbox_annotation_tab force_rnm
  end;

  fun cleanup (vs,es,bs) = let
    fun upd_vtab vtab = V.NTab.fold
          (fn (k,_) => if V.NSet.contains vs k then I else V.NTab.delete k)
          vtab vtab
    fun upd_etab etab = E.NTab.fold
          (fn (k,_) => if E.NSet.contains es k then I else E.NTab.delete k)
          etab etab
    fun upd_btab btab = B.NTab.fold
          (fn (k,_) => if B.NSet.contains bs k then I else B.NTab.delete k)
          btab btab
  in
    (update_vertex_annotation_tab upd_vtab)
    o (update_edge_annotation_tab upd_etab)
    o (update_bangbox_annotation_tab upd_btab)
  end;

  fun apply_renaming (vrnm,ernm,brnm) =
    (update_vertex_annotation_tab ((snd oo V.NTab.rename) vrnm))
    o (update_edge_annotation_tab ((snd oo E.NTab.rename) ernm))
    o (update_bangbox_annotation_tab ((snd oo B.NTab.rename) brnm));

  fun pretty ann =
    Pretty.chunks
      [Pretty.str "Graph Annotations {",
       Pretty.block
        [Pretty.str " ",
         Pretty.chunks
         [Pretty.block
          [Pretty.str "Graph Annotation: ",
           data_pretty (get_graph_annotation ann)],
          Pretty.block
          [Pretty.str "Vertex Annotations: ",
            V.NTab.pretty data_pretty (get_vertex_annotation_tab ann)],
           Pretty.block
          [Pretty.str "Edge Annotations: ",
           E.NTab.pretty data_pretty (get_edge_annotation_tab ann)],
            Pretty.block
          [Pretty.str "!-Box Annotations: ",
          B.NTab.pretty data_pretty (get_bangbox_annotation_tab ann)]]],
       Pretty.str "}"];

  val print = Pretty.writeln o pretty;
  val pretty_data = data_pretty;
  val print_data = Pretty.writeln o pretty_data;
end;

(**
 * Table of annotations on a graph
 *
 * Each component has an associated table of strings.
 *)
signature GRAPH_STRING_TABLE_ANNOTATIONS =
sig
  include GRAPH_ANNOTATIONS where type data = string Symtab.table;

  val lookup_graph_property : T -> string -> string option;
  val lookup_vertex_property : T -> V.name -> string -> string option;
  val lookup_edge_property : T -> E.name -> string -> string option;
  val lookup_bangbox_property : T -> B.name -> string -> string option;

  val set_graph_property : (string * string) -> T -> T;
  val set_vertex_property : V.name -> (string * string) -> T -> T;
  val set_edge_property : E.name -> (string * string) -> T -> T;
  val set_bangbox_property : B.name -> (string * string) -> T -> T;

  val remove_graph_property : string -> T -> T;
  val remove_vertex_property : V.name -> string -> T -> T;
  val remove_edge_property : E.name -> string -> T -> T;
  val remove_bangbox_property : B.name -> string -> T -> T;
end;

signature GRAPH_JSON_OBJECT_ANNOTATIONS =
  GRAPH_ANNOTATIONS where type data = Json.jobj

structure GraphStringTableAnnotations : GRAPH_STRING_TABLE_ANNOTATIONS =
struct
  fun pretty_symtab tab = let
    fun pretty_kv (k,v) = Pretty.str (" " ^ k ^ "=> " ^ v ^ ", ")
  in
    Pretty.chunks (map pretty_kv (Symtab.dest tab))
  end;

  structure Ann = GraphAnnotations(
    type data = string Symtab.table
    val data_init = Symtab.empty
    val data_pretty = pretty_symtab
  );
  open Ann;

  val lookup_graph_property = Symtab.lookup o Ann.get_graph_annotation;
  val lookup_vertex_property = Symtab.lookup oo Ann.get_vertex_annotation;
  val lookup_edge_property = Symtab.lookup oo Ann.get_edge_annotation;
  val lookup_bangbox_property = Symtab.lookup oo Ann.get_bangbox_annotation;

  val set_graph_property = Ann.update_graph_annotation o Symtab.update;
  fun set_vertex_property v = (Ann.update_vertex_annotation v) o Symtab.update;
  fun set_edge_property e = (Ann.update_edge_annotation e) o Symtab.update;
  fun set_bangbox_property b = (Ann.update_bangbox_annotation b) o Symtab.update;

  val remove_graph_property = Ann.update_graph_annotation o Symtab.delete;
  fun remove_vertex_property v = (Ann.update_vertex_annotation v) o Symtab.delete;
  fun remove_edge_property e = (Ann.update_edge_annotation e) o Symtab.delete;
  fun remove_bangbox_property b = (Ann.update_bangbox_annotation b) o Symtab.delete;
end;

structure GraphJsonObjectAnnotations : GRAPH_JSON_OBJECT_ANNOTATIONS = GraphAnnotations(
  type data = Json.jobj
  val data_init = Json.empty_obj
  fun data_pretty obj = Json.pretty (Json.Object obj)
)


