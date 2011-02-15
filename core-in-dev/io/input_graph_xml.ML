(* XMl Parsers for BANG_GRAPH, RG_VERTEX, and RG_EDGE *)
functor GraphInputXML (
  structure Graph : BANG_GRAPH
        and InVData : XML_INPUT 
        and InEData : XML_INPUT 
  sharing type InVData.data = Graph.OVertex.IData.data
  sharing type InEData.data = Graph.Edge.data
)  : XML_INPUT
= struct

  structure G = Graph;
  structure V = Graph.OVertex;
  structure E = Graph.Edge;
  structure B = Graph.BBox;
  
  type T = XML.tree;  (* XML.tree *)
  type data = Graph.T;

  open XML_Utils;
      
  fun input_boundary xml = 
      let val (_,_,children) = force_unpack_elem "boundary" xml
      in case children of
            [] => false
          | (x::xs) => input_boolean x
      end;

  fun input_vertex xml = 
      let val (_,_,children) = force_unpack_elem "vertex" xml
          val name = V.mk (input_name (get_required_child "name" xml))
          val (bnd,_) = get_elem false "boundary" children
      in
        case bnd of 
          NONE => (name, V.IVert (InVData.input xml))
        | SOME boundary => 
          let val b = input_boundary boundary
          in case b of false => (name, V.IVert (InVData.input xml))
                     | true => (name, V.EVert)
          end
      end;

  val input_source = force_get_text_from "source"
  val input_target = force_get_text_from "target"
      
  fun input_edge xml = 
      let val _ = force_unpack_elem "edge" xml
          val name = E.mk (input_name (get_required_child "name" xml))
          val source = V.mk (input_source (get_required_child "source" xml))
          val target = V.mk (input_target (get_required_child "target" xml))
          val edata = InEData.input xml
      in (name,edata,source,target) end;
  
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
  fun add_v (vname,vdata) g = 
      snd (G.add_named_vertex vname vdata g)
  
  (* FIXME: load up direction information *)
  fun add_e (ename,edata,src,tgt) g = 
      snd (G.add_named_edge ename (G.UnDirected,edata) src tgt g)
  
  fun add_bb (bname,bvlist) g = 
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
      in
        G.empty 
          |> fold add_v vertices 
          |> fold add_e edges
          |> fold add_bb  bboxes 
      end;
  
  val input = input_graph;			   

end (* structure InputXML *)


(* ------------------------------------------------------- *)
functor RGVertexInputXML (V : RG_VERTEX)  : XML_INPUT
= struct
  
  type T = XML.tree;
  type data = V.IData.data;
  
  open XML_Utils;
  
  fun input_rational xml  = 
      let val num = unpack_required_child "num" xml
          val denom = unpack_required_child "denom" xml
          val n = case num of 
                    (_,[]) => fail "Element <num> may not be empty."
                  | (_,(x::xs)) => input_int x
          val d = case denom of 
                    (_,[]) => fail "Element <denom> may not be empty."
                  | (_,(x::xs)) => input_int x
      in (n,d) end;
  
  fun input_constant xml =
      let val _= force_unpack_elem "constant" xml
      in input_rational xml end
  
  fun input_coeff xml = 
      let val _ = force_unpack_elem "coefficient" xml
      in input_rational xml end;
  
  fun input_variable xml = 
      let val _ = force_unpack_elem "variable" xml
          val name = get_required_child "name" xml
          val coeff = get_required_child "coefficient" xml
      in (input_name name, input_coeff coeff) end;
  
  fun input_angleexpr xml  =
      let val (_,_,children) = force_unpack_elem "angleexpr" xml
          val constant = case (get_elem false "constant" children) of
                           (NONE,_) => AngleExpr.Coeff.zero
                         | (SOME const,_) => input_constant const
          val vars = List.map input_variable 
                              (get_all_elems "variable" children)
      in AngleExpr.mk constant vars end;
  
  val input_colour = String.map Char.toLower o (force_get_text_from "colour");
  
  fun input xml =
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
      end;
end; (* functor RGGraphInputXML*)

functor RGEdgeInputXML (E : EDGE) :  XML_INPUT
= struct
  open XML_Utils;
  type T = XML.tree;
  type data = E.data;
  val input = K E.default_data;
end;

(* ---------------------------------------------------------- *)

structure RGGraphInputXML : XML_INPUT
  = GraphInputXML(
      structure Graph = RGGraph
      and InVData = RGVertexInputXML(RGGraph.RGVertex)
      and InEData = RGEdgeInputXML(RGGraph.Edge)
    );
