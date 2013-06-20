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
  val get_bbox_annotation : T -> B.name -> data;

  val get_vertex_annotation_tab : T -> data VTab.T;
  val get_edge_annotation_tab : T -> data ETab.T;
  val get_bbox_annotation_tab : T -> data BTab.T;

  val set_graph_annotation : data -> T -> T;
  val set_vertex_annotation : V.name -> data -> T -> T;
  val set_edge_annotation : E.name -> data -> T -> T;
  val set_bbox_annotation : B.name -> data -> T -> T;

  val update_graph_annotation : (data -> data) -> T -> T;
  val update_vertex_annotation : V.name -> (data -> data) -> T -> T;
  val update_edge_annotation : E.name -> (data -> data) -> T -> T;
  val update_bbox_annotation : B.name -> (data -> data) -> T -> T;

  val remove_vertex_annotation : V.name -> T -> T;
  val remove_edge_annotation : E.name -> T -> T;
  val remove_bbox_annotation : B.name -> T -> T;

  val rename_vertex : V.name -> V.name -> T -> T;
  val rename_edge : E.name -> E.name -> T -> T;
  val rename_bbox : B.name -> B.name -> T -> T;

  (* Arg1 is components to retain *)
  val cleanup : (V.NSet.T * E.NSet.T * B.NSet.T) -> T -> T;
  val apply_renaming : (VSub.T * ESub.T * BSub.T) -> T -> T;

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
      vertex_annotation_tab  : data VTab.T,
      edge_annotation_tab    : data ETab.T,
      bbox_annotation_tab : data BTab.T
    };

  val init = GraphAnn
    {
      graph_annotation    = data_init,
      vertex_annotation_tab  = VTab.empty,
      edge_annotation_tab    = ETab.empty,
      bbox_annotation_tab = BTab.empty
    };

  fun update_graph_annotation f (GraphAnn r) = GraphAnn {
    graph_annotation    = f(#graph_annotation r),
    vertex_annotation_tab  = #vertex_annotation_tab r,
    edge_annotation_tab    = #edge_annotation_tab r,
    bbox_annotation_tab = #bbox_annotation_tab r
  }

  fun update_vertex_annotation_tab f (GraphAnn r) = GraphAnn {
    graph_annotation    = #graph_annotation r,
    vertex_annotation_tab  = f(#vertex_annotation_tab r),
    edge_annotation_tab    = #edge_annotation_tab r,
    bbox_annotation_tab = #bbox_annotation_tab r
  }

  fun update_edge_annotation_tab f (GraphAnn r) = GraphAnn {
    graph_annotation    = #graph_annotation r,
    vertex_annotation_tab  = #vertex_annotation_tab r,
    edge_annotation_tab    = f(#edge_annotation_tab r),
    bbox_annotation_tab = #bbox_annotation_tab r
  }

  fun update_bbox_annotation_tab f (GraphAnn r) = GraphAnn {
    graph_annotation    = #graph_annotation r,
    vertex_annotation_tab  = #vertex_annotation_tab r,
    edge_annotation_tab    = #edge_annotation_tab r,
    bbox_annotation_tab = f(#bbox_annotation_tab r)
  }

  fun get_graph_annotation       (GraphAnn r) = #graph_annotation r
  fun get_vertex_annotation_tab  (GraphAnn r) = #vertex_annotation_tab r
  fun get_edge_annotation_tab    (GraphAnn r) = #edge_annotation_tab r
  fun get_bbox_annotation_tab (GraphAnn r) = #bbox_annotation_tab r

  fun set_graph_annotation    x  = update_graph_annotation    (fn _ => x)

  fun get_vertex_annotation ann v =
    case VTab.get_opt (get_vertex_annotation_tab ann) v
      of SOME x => x
       | NONE => data_init;

  fun get_edge_annotation ann e =
    case ETab.get_opt (get_edge_annotation_tab ann) e
      of SOME x => x
       | NONE => data_init;

  fun get_bbox_annotation ann e =
    case BTab.get_opt (get_bbox_annotation_tab ann) e
      of SOME x => x
       | NONE => data_init;

  fun set_vertex_annotation v d =
    update_vertex_annotation_tab (VTab.set (v,d));
  fun set_edge_annotation e d =
    update_edge_annotation_tab (ETab.set (e,d));
  fun set_bbox_annotation b d =
    update_bbox_annotation_tab (BTab.set (b,d));

  fun update_vertex_annotation v f ann =
    update_vertex_annotation_tab (VTab.map_default f data_init v) ann
  fun update_edge_annotation e f ann =
    update_edge_annotation_tab (ETab.map_default f data_init e) ann
  fun update_bbox_annotation b f ann =
    update_bbox_annotation_tab (BTab.map_default f data_init b) ann

  val remove_vertex_annotation = update_vertex_annotation_tab o VTab.delete;
  val remove_edge_annotation = update_edge_annotation_tab o ETab.delete;
  val remove_bbox_annotation = update_bbox_annotation_tab o BTab.delete;

  fun cleanup (vs,es,bs) = let
    fun upd_vtab vtab = VTab.fold
          (fn (k,_) => if V.NSet.contains vs k then I else VTab.delete k)
          vtab vtab
    fun upd_etab etab = ETab.fold
          (fn (k,_) => if E.NSet.contains es k then I else ETab.delete k)
          etab etab
    fun upd_btab btab = BTab.fold
          (fn (k,_) => if B.NSet.contains bs k then I else BTab.delete k)
          btab btab
  in
    (update_vertex_annotation_tab upd_vtab)
    o (update_edge_annotation_tab upd_etab)
    o (update_bbox_annotation_tab upd_btab)
  end;

  fun apply_renaming (vrn,ern,brn) ann =
  let
    val vrni = vrn |> VSub.extend_fixed (VTab.get_dom_set (get_vertex_annotation_tab ann)) |> VSub.inverse_of
    val erni = ern |> ESub.extend_fixed (ETab.get_dom_set (get_edge_annotation_tab ann)) |> ESub.inverse_of
    val brni = brn |> BSub.extend_fixed (BTab.get_dom_set (get_bbox_annotation_tab ann)) |> BSub.inverse_of
  in
    ann |> update_vertex_annotation_tab (fn m => VTab.compose (m,vrni))
        |> update_edge_annotation_tab   (fn m => ETab.compose (m,erni))
        |> update_bbox_annotation_tab   (fn m => BTab.compose (m,brni))
  end

  fun rename_vertex old new =
    apply_renaming (VSub.empty |> VSub.add (old,new), ESub.empty, BSub.empty)

  fun rename_edge old new =
    apply_renaming (VSub.empty, ESub.empty |> ESub.add (old,new), BSub.empty)

  fun rename_bbox old new = 
    apply_renaming (VSub.empty, ESub.empty, BSub.empty |> BSub.add (old,new))

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
            VTab.pretty data_pretty (get_vertex_annotation_tab ann)],
           Pretty.block
          [Pretty.str "Edge Annotations: ",
           ETab.pretty data_pretty (get_edge_annotation_tab ann)],
            Pretty.block
          [Pretty.str "!-Box Annotations: ",
          BTab.pretty data_pretty (get_bbox_annotation_tab ann)]]],
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

  val get_graph_property_opt  : T -> string -> string option;
  val get_vertex_property_opt : T -> V.name -> string -> string option;
  val get_edge_property_opt   : T -> E.name -> string -> string option;
  val get_bbox_property_opt   : T -> B.name -> string -> string option;

  val set_graph_property   : (string * string) -> T -> T;
  val set_vertex_property  : V.name -> (string * string) -> T -> T;
  val set_edge_property    : E.name -> (string * string) -> T -> T;
  val set_bbox_property : B.name -> (string * string) -> T -> T;

  val remove_graph_property  : string -> T -> T;
  val remove_vertex_property : V.name -> string -> T -> T;
  val remove_edge_property   : E.name -> string -> T -> T;
  val remove_bbox_property   : B.name -> string -> T -> T;
end;

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

  val get_graph_property_opt = Symtab.lookup o Ann.get_graph_annotation;
  val get_vertex_property_opt = Symtab.lookup oo Ann.get_vertex_annotation;
  val get_edge_property_opt = Symtab.lookup oo Ann.get_edge_annotation;
  val get_bbox_property_opt = Symtab.lookup oo Ann.get_bbox_annotation;

  val set_graph_property = Ann.update_graph_annotation o Symtab.update;
  fun set_vertex_property v = (Ann.update_vertex_annotation v) o Symtab.update;
  fun set_edge_property e = (Ann.update_edge_annotation e) o Symtab.update;
  fun set_bbox_property b = (Ann.update_bbox_annotation b) o Symtab.update;

  val remove_graph_property = Ann.update_graph_annotation o Symtab.delete;
  fun remove_vertex_property v = (Ann.update_vertex_annotation v) o Symtab.delete;
  fun remove_edge_property e = (Ann.update_edge_annotation e) o Symtab.delete;
  fun remove_bbox_property b = (Ann.update_bbox_annotation b) o Symtab.delete;
end;


