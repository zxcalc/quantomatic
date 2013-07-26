signature BANG_GRAPH_ISO_SHARING =
sig
  type T
  structure Graph : OGRAPH_SHARING
end

signature BANG_GRAPH_ISO =
sig
  type T
  structure Graph : BANG_GRAPH
  structure Sharing : BANG_GRAPH_ISO_SHARING
  sharing type Sharing.T = T
  sharing Sharing.Graph = Graph.Sharing

  val get_dom     : T -> Graph.T
  val get_cod     : T -> Graph.T
  val get_vmap    : T -> VSub.T
  val get_emap    : T -> ESub.T
  val get_bmap    : T -> BSub.T

  val mk     : Graph.T * Graph.T * VSub.T * ESub.T * BSub.T -> T
  val mk_opt : Graph.T * Graph.T * VSub.T * ESub.T * BSub.T -> T option
end

functor BangGraphIso(Graph : BANG_GRAPH) : BANG_GRAPH_ISO =
struct

  datatype T = BGIso of {
    dom  : Graph.T,
    cod  : Graph.T,
    vmap : VSub.T,
    emap : ESub.T,
    bmap : BSub.T
  }

  structure Graph = Graph

  structure Sharing =
  struct
    type T = T
    structure Graph = Graph.Sharing
  end

  exception bad_iso_exp of string

  fun get_dom  (BGIso r) = #dom r
  fun get_cod  (BGIso r) = #cod r
  fun get_vmap (BGIso r) = #vmap r
  fun get_emap (BGIso r) = #emap r
  fun get_bmap (BGIso r) = #bmap r

  fun mk (g, h, vmap, emap, bmap) =
  let
    (* note avoids data is not used. Also, we are allowed to define an iso more economically
     * by only mapping non-trivial name changes, hence "extend_fixed". This could fail if
     * implicitly-mapped names break injectivity. *)
    val vmap'  = vmap  |> VSub.clear_avoids
    val emap'  = emap  |> ESub.clear_avoids
    val bmap'  = bmap  |> BSub.clear_avoids
    val _      = if Graph.exact_eq (Graph.rename_bang_graph_anon (vmap', emap', bmap') g) h then ()
                 else raise bad_iso_exp "image of dom under iso not equal to cod"
                 handle VSub.name_clash_exp _ => raise bad_iso_exp "vmap not injective"
                      | ESub.name_clash_exp _ => raise bad_iso_exp "emap not injective"
                      | BSub.name_clash_exp _ => raise bad_iso_exp "bmap not injective"
  in BGIso { dom=g, cod=h, vmap=vmap', emap=emap', bmap=bmap' }
  end

  fun mk_opt input = SOME (mk input)
    handle bad_iso_exp _ => NONE
end