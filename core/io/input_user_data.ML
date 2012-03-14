(* FIXME: structure used is a bit squanky and ineffeccient. *)
functor InputUIDataXML(
  structure UIData : USER_DATA
)  : INPUT_XML
= struct
  open XML_Parse_Utils; 

  structure UD = UIData;
  
  type T =  XML.tree;  (* XML.tree *)
  type data = UIData.T;

  fun data_to_string _ = "[UserData]";

  fun input_entry xml =
    let
     val _ = assert_tag "entry" xml
     val child = get_unique_child xml
     val k = (get_attribute xml "name") 
        handle bad_input_exp (s,p) => raise
                      bad_input_exp (s,"entry/"^p)
     val data = get_text child
    in
      (k, data) 
    end;


fun input_element mk tag xml = 
  let
    val _ = assert_tag tag xml
    val name = (get_attribute xml "name")
      handle bad_input_exp (s,p) => raise
                    bad_input_exp (s,tag^"/"^p)
    val children = get_children xml
    val mk_tab = Symtab.make
  in
    (mk name, mk_tab (map input_entry (filter_elems "entry" children)))
      handle bad_input_exp (s,p) => raise bad_input_exp
        (s,"element_uidata("^name^")/"^p)
  end;


  val input_vud =  input_element (V.mk) "vertex_userdata";
  val input_eud =  input_element (E.mk) "edge_userdata";
  val input_bud =  input_element (B.mk) "bangbox_userdata";

  fun add_g_ud g_ud ud = UD.update_g_ud (fn _ => g_ud) ud 
  fun add_v_ud (n, vud) ud = UD.update_v_ud (fn _ => vud) n ud 
  fun add_e_ud (n, eud) ud = UD.update_e_ud (fn _ => eud) n ud
  fun add_b_ud (n, bud) ud = UD.update_bb_ud (fn _ => bud) n ud

  fun input_graph_ud xml =
    let
      val () = assert_tag "graph_data" xml
      val children = get_children xml
      val mk_tab = Symtab.make  
    in
      mk_tab (map input_entry (filter_elems "entry" children))
    end;

  (* There may or may not be user_data but if there
   * are then we must have a graph_and_user_data tag*)
  fun input_ud xml = if not (test_tag "graph_and_user_data" xml)
    then UIData.init
    else
      let
        val xml' = List.last (get_children xml) (*The user data is the last  *)
        val () = assert_tag "user_data" xml'
        val children = get_children xml'
        val graph_ud = input_graph_ud (get_unique_elem "graph_data" children)
        (* Better to use a functor here, but not really worth it *)
        val vs_ud = List.map input_vud (filter_elems "vertex_userdata" children)
        val es_ud = List.map input_eud (filter_elems "edge_userdata" children)
        val bs_ud = List.map input_bud (filter_elems "bangbox_userdata" children)
      in
        UD.init
          |> add_g_ud graph_ud
          |> fold add_v_ud vs_ud
          |> fold add_e_ud es_ud
          |> fold add_b_ud bs_ud
      end
      handle bad_input_exp (s,p) => raise bad_input_exp (s, "user_data/"^p);

  val input = input_ud;
			   
end; (* structure InputXML *)

(* vim:et:sts=2:sw=2
*)
