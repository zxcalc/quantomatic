structure InputStringXML : INPUT_XML where type data = string
= struct

  open XML_Parse_Utils; 
  
  type T = XML.tree;  (* XML.tree *)
  type data = string;

  val data_to_string = (fn x => x);
  
  fun input xml  =
    (let val s = get_unique_child_text xml ;
     in s end)
      handle bad_input_exp (s,p) => raise bad_input_exp (s,"string data/"^p);

end;
