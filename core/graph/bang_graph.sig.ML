signature BANG_GRAPH =
sig
  include OGRAPH
  
  exception bbox_not_open_exp of (V.name * V.name) * B.name * T
  exception bbox_already_exists_exp of B.name * T
  exception no_such_bbox_exp of string * B.name * T
  exception bbox_bad_parent_exp of B.name * B.name * T
  exception merge_bbox_exp of string
  
  
  val has_bbox : T -> B.name -> bool
  val get_bboxes : T -> B.NSet.T
  (* Does not contain empty !-boxes (or unboxed vertices) *)
  val get_bbox_rel : T -> BVRel.T
  (* Return the "parent" function for nested bang-boxes *)
  (*val get_bbox_parent_map : T -> BBFn.T*)
  
  (* all vertices that are in at least one !-box *)
  val get_bboxed : T -> V.NSet.T
  (* all vertices that are in at no !-boxes *)
  val get_unbboxed : T -> V.NSet.T
  
  (* no_such_vname_exp if vertex does not exist *)
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
  
  val get_bbox_parents : T -> B.name -> B.NSet.T
  val get_bbox_children : T -> B.name -> B.NSet.T
  
  (* returns true if bbox has parent(s) *)
  val bbox_has_parents : T -> B.name -> bool

  val bbox_has_children : T -> B.name -> bool
  
  (* we don't copy updaters/setters for internal structures,
   * since using these can lead to an inconsistent state *)
  
  (* no_such_bbox_exp if bbox does not exist *)
  val get_vertices_in_bbox : T -> B.name -> V.NSet.T
  
  (* no_such_vname_exp if vertex does not exist *)
  val get_bboxes_containing_vertex : T -> V.name -> B.NSet.T
  
  val try_rename1_bbox : B.name -> B.name -> T -> T option
  
  (* no_such_bbox_exp *)
  val kill_bbox : B.name -> T -> T
  (* no_such_bbox_exp *)
  val drop_bbox : B.name -> T -> T
  
  val merge_bboxes : B.NSet.T -> T -> B.name * T

  val expand_bbox : B.name -> (VSub.T * ESub.T * BSub.T) * T
                           -> (VSub.T * ESub.T * BSub.T) * T

  val copy_bbox : B.name -> (VSub.T * ESub.T * BSub.T) * T
                         -> (VSub.T * ESub.T * BSub.T) * T
  
  (*val fresh_copy_bbox : B.name -> T -> (B.name * T)
  val fresh_copy_bbox_anon : B.name -> T -> T*)
  
  (*

  exception merge_bbox_exp of string
  (* thrown when using old API on a graph with overlapping !-boxes *)

  
  val merge_bboxes : B.NSet.T -> T -> B.name * T

  (* copy a set of vertices, making copy having names fresh w.r.t.
     vrn and ern (edge and vertex renaming).
     copies edges to all vertices outside the selected set.
     assumes: (get_vnames g) <= get_avoids(vrn)
              (get_enames g) <= get_avoids(ern)
              (get_bboxes g) <= get_avoids(brn)
  *)
  val copy1_bbox : B.name (* bbox to copy *)
        -> ((V.renaming (* renaming for bbox copy/avoids *)
             * E.renaming
             * B.renaming)
            * T) (* graph containing bbox name *)
        -> (B.name (* new box name *)
            * ((V.renaming (* how vnames were copied *)
                 * E.renaming (* ename copying *)
                 * B.renaming) (* bbname copying *)
               * T)) (* new graph with bbox copied *)

  val fresh_copy1_bbox : B.name (* bbox to copy *)
        -> T (* graph containing bbox name *)
        -> (B.name (* new box name *)
            * T) (* new graph with bbox copied *)

  (* copy and drop *)
  val expand1_bbox : B.name (* bbox to expand *)
        -> ((V.renaming (* renaming for bbox copy/avoids *)
             * E.renaming
             * B.renaming)
            * T) (* graph containing bbox name *)
        -> ((V.renaming (* how vnames were copied *)
                 * E.renaming (* ename copying *)
                 * B.renaming) (* bbname copying *)
               * T) (* new graph with bbox expanded *)

  (* copy and drop *)
  val fresh_expand1_bbox : B.name (* bbox to expand *)
        -> T (* graph containing bbox name *)
        -> T (* new graph with bbox expanded *)
  
  (* throws bbox_already_exists_exp on name clash *)
  (* identity if original name does not exist *)
  val rename_bbox : B.name -> B.name -> T -> T
  
  *)
  
  
  (************************)
  (****** DEPRECATED ******)
  (************************)
  
  (*
  (* Does not contain empty !-boxes *)
  val get_bbox_table : T -> V.NSet.T B.NTab.T
  (* Does not contain unboxed vertices *)
  val get_vertex_bbox_table : T -> B.NSet.T V.NTab.T
  
  *** Now called add_named_bbox:
  val add_bbox_named_like : B.name -> T -> (B.name * T)
  
  
  *** These two are replaced by set_bbox_parent_opt
  
  (* set parent of the first bbox to be the second bbox *)
  (* snd will be the parent of the fst *)
  val set_bbox_parent : B.name -> B.name -> T -> T

  (* make the given bbox an orphan *)
  val clear_bbox_parent : B.name -> T -> T
  
  *** Now called get_bbox_parent_opt
  val lookup_bbox_parent : T -> B.name -> B.name option

  *** Deprecate as many "convert-to-list" wrappers as possible
  val get_bbox_list : T -> B.name list
  
  
  *** Seems a bit specific for a library method. Can probably be done with
      assert_open_subgraph, etc...
  
  (* If adding an edge between the two given vertices would extend
   * one or more !-boxes, return one such !-box, otherwise NONE
   *)
  val tryget_bbox_extended_by_edge : V.name -> V.name -> T -> B.name option
  
  *** Now called get_vertices_in_bbox
  (* no_such_bbox_exp if bbox does not exist *)
  val get_bbox_contents : T -> B.name -> V.NSet.T
  
  *** Now called get_bboxes_containing_vertex
  (* getters for vert -> bbox *)
  (* no_such_vname_exp if vertex does not exist *)
  val get_bboxes_of : T -> V.name -> B.NSet.T
  
  *** Now called add_bbox_anon
  val doadd_bbox : T -> T
  
  *** Implement me in controller
  val remove_from_all_bboxes : V.NSet.T -> T -> T
  
  *)
end