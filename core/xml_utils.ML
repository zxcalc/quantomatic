structure XML_Utils =
struct

open XML;

exception Bad_input_exp of string;
	  
fun fail msg = raise Bad_input_exp msg
		     
val tolower = String.map Char.toLower
val toupper = String.map Char.toUpper

fun force_unpack_elem wanted xml = 
    case xml of
      Text  _ => fail ("Expected <"^ wanted ^"> element but saw text.")
    | Output _ => fail ("Expected <"^ wanted ^"> element but saw output.")
    | Elem (elem,attr,children) => 
      if ((tolower elem) = 
	  (tolower wanted)) 
      then (elem, attr,children) 
      else fail ("Expected <"^ wanted ^"> element but saw <"^elem^">.")
	   
fun test_elem wanted test = 
    let val  _ = force_unpack_elem wanted test in true end
    handle Bad_input_exp _ => false
			      
(* get the first element in the list  which is of type "wanted"
   return the rest of the list in the second coord
   if none is found either return NONE or fail depending 
   on value of boolean param force
*)
fun get_elem true elem [] = fail ("Required element <"^elem^"> not found.")
  | get_elem false _ [] = (NONE, [])
  | get_elem force wanted (xml::rest) = 
    if (test_elem wanted xml) 
    then (SOME xml, rest)				
    else let val (v,l) = get_elem force wanted rest in (v,xml::l) end

fun get_all_elems wanted =
    List.filter (test_elem wanted)

fun get_child force child xml = 
    case xml of 
      Text _ => fail ("Can't get children of Text")
    | Output _ => fail ("Can't get children of output") 
    | Elem (_,_,children) => get_elem force child children

fun get_required_child child xml = 
    case xml of 
      Text _ => fail ("Can't get children of Text")
    | Output _ => fail ("Can't get children of output") 
    | Elem (elem,_,_) =>
      case get_child false child xml of 
	(NONE,_) => fail ("Element <"^elem^"> requires child element <"
		      ^child^"> which is missing.")
      | (SOME thing,_) => thing
	
fun unpack_elem elem =
    case elem of 
      Text _ => fail "Can't unpack text"
    | Output _ => fail "Can't unpack output"
    | Elem thing => thing

fun unpack_required_child child xml = unpack_elem (get_required_child child xml)

fun get_text xml = 
    case xml of 
      Text t => t
    | Output _ => fail ("Expected text but saw output.")
    | Elem (elem,_,_) => fail ("Expected text but saw <"^ elem ^"> element.")

fun force_get_text_from elemkind xml = 
    let val (_,_,children) = force_unpack_elem elemkind xml
    in
      case children of
	[] => fail ("Element <"^elemkind^"> must not be empty.")
      | (child::rest) => get_text child
    end

val input_name = force_get_text_from "name"

fun input_boolean xml = 
    let val t = tolower (get_text xml)
    in
      if (t = "true") then true
      else if (t = "false") then false
      else raise fail ("Expected true or false but saw \""^t^"\".")
    end

fun input_int xml = 
    let val t = tolower (get_text xml) 
    in
      case Int.fromString t of 
	SOME i => i 
      | NONE => fail ("Expected integer but saw \""^t^"\".")
    end

end
