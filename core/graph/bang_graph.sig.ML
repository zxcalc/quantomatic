signature BANG_GRAPH =
sig
  include OGRAPH
  
  exception bbox_not_open_exp of (V.name * V.name) * B.name * T
  exception duplicate_bbox_exp of B.name * T
  exception no_such_bbox_exp of string * B.name * T
  exception bbox_bad_parent_exp of B.name * B.name * T
  exception merge_bbox_exp of string
  
  
  val has_bbox : T -> B.name -> bool
  val get_bboxes : T -> B.NSet.T
  (* Does not contain empty !-boxes (or unboxed vertices) *)
  (*val get_bbox_rel : T -> BVRel.T*)
  (* Return the mapping from parent !-boxes to child !-boxes *)
  (*val get_bbox_child_rel : T -> BBRel.T*)
  
  (* all vertices that are in at least one !-box *)
  val get_bboxed : T -> V.NSet.T
  (* all vertices that are in at no !-boxes *)
  val get_unbboxed : T -> V.NSet.T
  
  (* no_such_vertex_exp if vertex does not exist *)
  val is_bboxed : T -> V.name -> bool
  
  val get_empty_bboxes : T -> B.NSet.T
  
  val add_named_bbox : B.name -> T -> T
  val add_bbox : T -> (B.name * T)
  val add_bbox_anon : T -> T
  
  (* does not delete the contained vertices *)
  (* identity if name does not exist *)
  val delete_bbox : B.name -> T -> T
  
  (* no_such_bbox_exp if !-box does not exist *)
  (* The minimum open subgraph containing the
   * given vertices is added, to ensure the
   * !-box remains an open subgraph.
   *
   * Returns the actual vertices added (always
   * a superset of the vertex set given).
   *
   * TODO: improve name? add_subgraph_to_bbox or something...
   *)
  val add_to_bbox : B.name -> V.NSet.T -> T -> (V.NSet.T * T)
  val add_to_bbox_anon : B.name -> V.NSet.T -> T -> T
  val remove_from_all_bboxes : V.NSet.T -> T -> T
  
  (* no_such_bbox_exp if !-box does not exist *)
  (* does not raise if verts are not in !-box *)
  (* Does not remove any vertices that are
   * necessary for the !-box to remain open
   *
   * Returns the actual vertices removed (always
   * a subset of the vertex set given).
   *)
  val remove_from_bbox : B.name -> V.NSet.T -> T -> (V.NSet.T * T)
  val remove_from_bbox_anon : B.name -> V.NSet.T -> T -> T
  
  
  (* Set parent of the first bbox to be the second bbox, or clears
   * when the second arg is NONE. *)
  (*val set_bbox_parent_opt : B.name -> B.name option -> T -> T*)
  
  (* raises no_such_bbox_exn if parent or children not in graph *)
  val add_children_to_bbox : B.name -> B.NSet.T -> T -> T
  val add_child_to_bbox : B.name -> B.name -> T -> T
  
  val get_bbox_parents : T -> B.name -> B.NSet.T
  val get_bbox_children : T -> B.name -> B.NSet.T
  
  (* returns true if bbox has parent(s) *)
  val bbox_has_parents : T -> B.name -> bool

  val bbox_has_children : T -> B.name -> bool
  
  (* we don't copy updaters/setters for internal structures,
   * since using these can lead to an inconsistent state *)
  
  (* no_such_bbox_exp if bbox does not exist *)
  val get_vertices_in_bbox : T -> B.name -> V.NSet.T
  
  (* no_such_vertex_exp if vertex does not exist *)
  val get_bboxes_containing_vertex : T -> V.name -> B.NSet.T
  
  (* no_such_bbox_exp *)
  val kill_bbox : B.name -> T -> T
  (* no_such_bbox_exp *)
  val drop_bbox : B.name -> T -> T
  
  val merge_bboxes : B.NSet.T -> T -> B.name * T
  
  val expand_bbox : B.name -> (VSub.T * ESub.T * BSub.T) * T
                           -> (VSub.T * ESub.T * BSub.T) * T
  
  val copy_bbox : B.name -> (VSub.T * ESub.T * BSub.T) * T
                         -> (VSub.T * ESub.T * BSub.T) * T
  
  
  val fresh_copy_bbox : B.name -> T -> (VSub.T * ESub.T * BSub.T) * T
  val fresh_copy_bbox_anon : B.name -> T -> T
  val fresh_expand_bbox : B.name -> T -> (VSub.T * ESub.T * BSub.T) * T
  val fresh_expand_bbox_anon : B.name -> T -> T
  
  val rename_bang_graph      : (VSub.T * ESub.T * BSub.T) -> T ->
                               (VSub.T * ESub.T * BSub.T) * T
  val rename_bang_graph_anon : (VSub.T * ESub.T * BSub.T) -> T -> T
  val rename_bboxes          : BSub.T -> T -> T
  val rename_bbox            : B.name -> B.name -> T -> T
  val rename_bbox_opt        : B.name -> B.name -> T -> T option
end
