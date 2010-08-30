(* 
 XMl Parsers for BANG_GRAPH, RG_VERTEX, and RG_EDGE
					    rwd 11-jan-2009
*)

functor GraphInputXML (structure Graph : BANG_GRAPH
                       structure InVData : INPUT_VDATA 
                       where type intype = XML.tree
                       structure InEData : INPUT_EDATA 
                       where type intype = XML.tree
  sharing InVData.V.SharingWithIData = Graph.OVertex.SharingWithIData
  sharing InEData.E.EdgeSharingWithData = Graph.Edge.EdgeSharingWithData
)  : INPUT_GRAPH
= struct
  structure G = Graph;
  structure V = Graph.OVertex;
  structure E = Graph.Edge;
  structure B = Graph.BBox;
  structure BR = Graph.VtoBoxMap;

  type graph = Graph.T;
  type vertex = V.data;
  type edge = E.data;
  type vname = V.name;
  type ename = E.name;
  type bboxname = B.name;

  open XML_Utils;

  type intype = tree;  (* XML.tree *)
  type outtype = graph;

  val input_vdata = InVData.input
  val input_edata = InEData.input
      
  fun input_boundary xml = 
      let val (_,_,children) = force_unpack_elem "boundary" xml
      in
	case children of
	  [] => false
	| (x::xs) => input_boolean x
      end

  fun input_vertex xml = 
      let val (_,_,children) = force_unpack_elem "vertex" xml
	  val name = V.mk (input_name (get_required_child "name" xml))
	  val (bnd,_) = get_elem false "boundary" children
      in
	case bnd of 
	  NONE => (name, V.IVert (input_vdata xml))
	| SOME boundary => 
	  let val b = input_boundary boundary
	  in
	    case b of false => (name, V.IVert (input_vdata xml))
		    | true => (name, V.EVert)
	  end
      end
  
val input_source = force_get_text_from "source"
val input_target = force_get_text_from "target"
      
    
  fun input_edge xml = 
      let val _ = force_unpack_elem "edge" xml
	  val name = E.mk (input_name (get_required_child "name" xml))
	  val source = V.mk (input_source (get_required_child "source" xml))
	  val target = V.mk (input_target (get_required_child "target" xml))
	  val edata = input_edata xml
      in
	(name,edata,source,target)
      end    

val input_boxedvertex = V.mk o (force_get_text_from "boxedvertex")

fun input_bbox xml =
    let val (_,_,children) = force_unpack_elem "bangbox" xml
	val name = B.mk (input_name (get_required_child "name" xml))
	val bangverts = List.map input_boxedvertex 
				 (get_all_elems "boxedvertex" children)
    in
      (name,bangverts)
    end

(* uncurry some graph methods *)
fun add_v ((vname,vdata),g) = 
    snd (G.add_named_vertex vname vdata g)

fun add_e ((ename,edata,src,tgt),g) = 
    snd (G.add_named_edge ename edata src tgt g)

fun add_bb ((bname,bvlist),g) = 
    let val (realname, g') = G.new_box bname g
	val vset  = V.NSet.of_list bvlist
    in
      G.move_to_box realname vset g'
    end

fun input_graph xml =
    let val (_,_,children) = force_unpack_elem "graph" xml
	val vertices = List.map input_vertex (get_all_elems "vertex" children)
	val edges = List.map input_edge (get_all_elems "edge" children)
	val bboxes = List.map input_bbox (get_all_elems "bangbox" children)
	val g = foldl add_v G.empty vertices
	val g'= foldl add_e g edges
    in
      foldl add_bb g' bboxes
    end

val input = input_graph;			   

end (* structure InputXML *)



(* ------------------------------------------------------- *)

functor RGVertexInputXML (Graph : RG_GRAPH)  : INPUT_VDATA 
=
struct

structure V = Graph.RGVertex
type outtype = V.IData.data;
     
open XML_Utils;

type intype = tree;

fun input_rational xml  = 
    let val num = unpack_required_child "num" xml
	val denom = unpack_required_child "denom" xml
    in
      let val n = case num of 
		    (_,_,[]) => fail "Element <num> may not be empty."
		  | (_,_,(x::xs)) => input_int x
	  val d = case denom of 
		    (_,_,[]) => fail "Element <denom> may not be empty."
		  | (_,_,(x::xs)) => input_int x
      in
	(n,d)
      end
    end

fun input_constant xml =
    let val _= force_unpack_elem "constant" xml
    in
      input_rational xml
    end

fun input_coeff xml = 
    let val _ = force_unpack_elem "coefficient" xml
    in
      input_rational xml
    end

fun input_variable xml = 
    let val _ = force_unpack_elem "variable" xml
    in
      let val name = get_required_child "name" xml
	  val coeff = get_required_child "coefficient" xml
      in
	(input_name name, input_coeff coeff)
      end
    end

fun input_angleexpr xml  =
    let val (_,_,children) = force_unpack_elem "angleexpr" xml
    in
      let val constant = case (get_elem false "constant" children) of
			   (NONE,_) => AngleExpr.Coeff.zero
			 | (SOME const,_) => input_constant const
	  val vars = List.map input_variable 
			      (get_all_elems "variable" children)
      in
	      AngleExpr.mk constant vars
      end
    end

val input_colour = tolower o (force_get_text_from "colour")

fun input_vdata xml =
    let val (_,_,children) = force_unpack_elem "vertex" xml
	      val col = input_colour (get_required_child "colour" xml)
    in 
      if (col = "h") then V.Hnd
      else
	      let val (aopt,_) = get_elem false "angleexpr" children
	          val angle = case aopt of NONE => AngleExpr.zero
				                           | SOME a => input_angleexpr a
	      in
	        if (col = "green") then V.Znd angle
	        else if (col = "red") then V.Xnd angle
	        else fail ("Unknown colour : "^col^".")
	      end
    end
    
val input = input_vdata

end (* functor RGGraphInputXML*)

functor RGEdgeInputXML (Graph : RG_GRAPH) :  INPUT_EDATA 
=
struct

structure E = Graph.Edge
type outtype = E.data

open XML_Utils;
type intype = tree;

fun input_edata _ = E.default_data

val input = input_edata

end

(* ---------------------------------------------------------- *)

structure RGGraphInputXML : INPUT_GRAPH
  = GraphInputXML(
      structure Graph = RGGraph
      and InVData = RGVertexInputXML (RGGraph)
      and InEData = RGEdgeInputXML (RGGraph)
    );
