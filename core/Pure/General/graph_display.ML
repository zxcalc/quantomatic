(*  Title:      Pure/General/graph_display.ML
    Author:     Makarius

Support for graph display.
*)

signature GRAPH_DISPLAY =
sig
  type node
  val content_node: string -> Pretty.T list -> node
  val session_node: {name: string, unfold: bool, directory: string, path: string} -> node
  type entry = (string * node) * string list
  val display_graph: entry list -> unit
  val display_graph_old: entry list -> unit
end;

structure Graph_Display: GRAPH_DISPLAY =
struct

(* graph entries *)

datatype node =
  Node of {name: string, content: Pretty.T list, unfold: bool, directory: string, path: string};

fun content_node name content =
  Node {name = name, content = content, unfold = true, directory = "", path = ""};

fun session_node {name, unfold, directory, path} =
  Node {name = name, content = [], unfold = unfold, directory = directory, path = path};

type entry = (string * node) * string list;


(* display graph *)

local

fun encode_node (Node {name, content, ...}) =
  (name, content) |>
    let open XML.Encode
    in pair string (YXML.parse_body o Pretty.symbolic_string_of o Pretty.chunks) end;

val encode_graph =
  let open XML.Encode in list (pair (pair string encode_node) (list string)) end;

in

fun display_graph entries =
  let
    val ((bg1, bg2), en) =
      YXML.output_markup_elem
        (Active.make_markup Markup.graphviewN {implicit = false, properties = []});
  in writeln ("See " ^ bg1 ^ YXML.string_of_body (encode_graph entries) ^ bg2 ^ "graph" ^ en) end;

end;


(* support for old browser *)

local

structure Graph =
  Graph(type key = string * string val ord = prod_ord string_ord string_ord);

fun build_graph entries =
  let
    val ident_names =
      fold (fn ((ident, Node {name, ...}), _) => Symtab.update_new (ident, (name, ident)))
        entries Symtab.empty;
    val the_key = the o Symtab.lookup ident_names;
  in
    Graph.empty
    |> fold (fn ((ident, node), _) => Graph.new_node (the_key ident, node)) entries
    |> fold (fn ((ident, _), parents) =>
        fold (fn parent => Graph.add_edge (the_key parent, the_key ident)) parents) entries
  end;

val sort_graph = build_graph #> (fn graph =>
  Graph.topological_order graph |> map (fn key =>
    let val (_, (node, (preds, _))) = Graph.get_entry graph key
    in ((#2 key, node), map #2 (Graph.Keys.dest preds)) end));

val encode_browser =
  sort_graph
  #> map (fn ((key, Node {name, unfold, content = _, directory, path}), parents) =>
    "\"" ^ name ^ "\" \"" ^ key ^ "\" \"" ^ directory ^ (if unfold then "\" + \"" else "\" \"") ^
    path ^ "\" > " ^ space_implode " " (map quote parents) ^ " ;")
  #> cat_lines;

in

fun display_graph_old entries =
  let
    val ((bg1, bg2), en) =
      YXML.output_markup_elem
        (Active.make_markup Markup.browserN {implicit = false, properties = []});
  in writeln ("See " ^ bg1 ^ encode_browser entries ^ bg2 ^ "graph" ^ en) end;

end;

end;
