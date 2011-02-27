(* XMl Parsers for BANG_GRAPH, RG_VERTEX, and RG_EDGE *)
functor InputGraphXML (
  structure Graph : BANG_GRAPH
        and InVData : XML_INPUT 
        and InEData : XML_INPUT 
  sharing type InVData.data = Graph.OVertex.IData.data
  sharing type InEData.data = Graph.Edge.data
)  : XML_INPUT
= struct
  open XML_Parse_Utils; 

  structure G = Graph;
  structure V = Graph.OVertex;
  structure E = Graph.Edge;
  structure B = Graph.BBox;
  
  type T = XML.tree;  (* XML.tree *)
  type data = Graph.T;

      
  fun input_vertex xml = 
      let 
        val () = assert_tag "vertex" xml
        val children = get_children xml
        val name = V.mk (get_text (get_the_elem "name" children))
      in
        case lookup_the_elem "boundary" children of 
          NONE => (name, V.IVert (InVData.input xml))
        | SOME boundary_xml => 
          if InputBoolXML.input (get_the_child boundary_xml) then (name, V.EVert)
          else (name, V.IVert (InVData.input xml)) 
      end;

(* 
  val input_source = force_get_text_from "source"
  val input_target = force_get_text_from "target"
*)
      
  fun input_edge xml = 
      let 
        val _ = assert_tag "edge" xml
        val children = get_children xml 
        val name = E.mk (get_the_child_text (get_the_elem "name" children))
        val source = V.mk (get_the_child_text (get_the_elem "source" children))
        val target = V.mk (get_the_child_text (get_the_elem "target" children))
        val edata = InEData.input xml
      in (name,edata,source,target) end;
  
  fun input_bbox xml =
      let 
        val _ = assert_tag "bangbox" xml
        val children = get_children xml 
        val name = B.mk (get_the_child_text (get_the_elem "name" children))
        val bangverts = List.map (V.mk o get_the_child_text) 
                                 (filter_elems "boxedvertex" children)
      in (name,bangverts) end;
  
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
      let 
        val () = assert_tag "graph" xml
        val children = get_children xml
        val vertices = List.map input_vertex (filter_elems "vertex" children)
        val edges = List.map input_edge (filter_elems "edge" children)
        val bboxes = List.map input_bbox (filter_elems "bangbox" children)
      in
        G.empty 
          |> fold add_v vertices 
          |> fold add_e edges
          |> fold add_bb bboxes 
      end;
  
  val input = input_graph;
			   
end (* structure InputXML *)


(* ------------------------------------------------------- *)
functor InputRGVertexXML (V : RG_VERTEX)  : XML_INPUT
= struct
  open XML_Parse_Utils; 
  
  type T = XML.tree;
  type data = V.IData.data;
  
  fun input_rational xml = 
      let val children = get_children xml;
          val num = get_the_child (get_the_elem "num" children)
          val denom = get_the_child (get_the_elem "denom" children)
      in (InputIntXML.input num, InputIntXML.input denom) end;
  
  fun input_constant xml =
      let val () = assert_tag "constant" xml
      in input_rational xml end
  
  fun input_coeff xml = 
      let val () = assert_tag "coefficient" xml
      in input_rational xml end;
  
  fun input_variable xml = 
      let val _ = assert_tag "variable" xml
          val children = get_children xml
          val name = get_the_child_text (get_the_elem "name" children)
          val coeff = get_the_elem "coefficient" children
      in (name, input_coeff coeff) end;
  
  fun input_angleexpr xml  =
      let val () = assert_tag "angleexpr" xml
          val children = get_children xml 
          val constant = case (lookup_the_elem "constant" children) of
                           (NONE) => AngleExpr.Coeff.zero
                         | (SOME const) => input_constant const
          val vars = List.map input_variable (filter_elems "variable" children)
      in AngleExpr.mk constant vars end;
    
  fun input xml =
      let val () = assert_tag "vertex" xml
          val children = get_children xml
          val col = (String.map Char.toLower o get_the_child_text 
                     o get_the_elem "colour") children
      in 
        if (col = "h") then V.Hnd
        else
          let 
            val angle = 
                case lookup_the_elem "angleexpr" children of NONE => AngleExpr.zero
                   | SOME a => input_angleexpr a
          in
            if (col = "green") then V.Znd angle
            else if (col = "red") then V.Xnd angle
            else raise bad_input_exp ("Unknown colour : "^col^".")
          end
      end;
end; (* functor RGGraphInputXML*)

functor InputRGEdgeXML (E : EDGE) :  XML_INPUT
= struct
  open XML_Parse_Utils;
  type T = XML.tree;
  type data = E.data;
  val input = K E.default_data;
end;

(* ---------------------------------------------------------- *)

structure InputRGGraphXML : XML_INPUT
  = InputGraphXML(
      structure Graph = RGGraph
      and InVData = InputRGVertexXML(RGGraph.RGVertex)
      and InEData = InputRGEdgeXML(RGGraph.Edge)
    );
