signature INPUT =
sig
  type intype
  type outtype
  val input : intype -> outtype
  exception Bad_input_exp of string;
end

signature INPUT_GRAPH =
sig

  structure G : BANG_GRAPH

(*
  type graph = G.T (* a graph *)
  type vertex = G.Vertex.data
  type edge = G.Edge.data
  type bboxname = G.BBox.name
  type ename = G.Edge.name
  type vname = G.Vertex.name
*)
  include INPUT where type outtype = G.T
 (*
  val input_graph : intype -> graph
  val input_edge  :  intype -> (ename * edge * vname * vname)
  val input_vertex  : intype -> (vname * vertex)
  val input_bbox : intype -> (bboxname * vname list)
*)
end

signature INPUT_VDATA =
sig
  structure V : BVERTEX
  include INPUT where type outtype = V.IData.data
end

signature INPUT_EDATA =
sig
  structure E : EDGE
  include INPUT where type outtype = E.data
end


(* -------------------------------------------------------- *)

