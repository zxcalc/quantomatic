(* *)

(*-------------------------------------------------------*)
structure OutputStringXML : OUTPUT_XML where type data = string 
= struct 
  type T = XML.tree;
  type data = string;

  open XML;
  open XML_Output_Utils;
  
  fun output (s) = 
    wrap_text_simple "string_of" (s);
end; (* structure *)

(* vim:et:sts=2:sw=2
*)
