(* TODO: provide link to visualisation XML *)
(* val rg_gui_spec_xml = XMLReader.read_from_file "gui.xml"; *)


(*  *)
structure Test_VertexComponentData_Param
: ELEMENT_COMPONENT_DATA_PARAM where type data = Test_RG_InternVData.data
= struct 
  structure VData = Test_RG_InternVData;
  type data = VData.data;
  exception unknown_type_exp of string;
  exception unexpected_data_exp;

  fun type_of (VData.Xnd _) = "X"
    | type_of (VData.Znd _) = "Z"
    | type_of (VData.Hnd)   = "hadamard";

  fun cdata_of (VData.Xnd a) = (ComponentData.LinratAngleExpr a)
    | cdata_of (VData.Znd a) = (ComponentData.LinratAngleExpr a)
    | cdata_of (VData.Hnd)   = ComponentData.Unit;

  fun default_data "X" = (VData.Xnd LinratAngleExpr.zero)
    | default_data "Z" = (VData.Znd LinratAngleExpr.zero)
    | default_data "hadamard" = (VData.Hnd)
    | default_data s = raise unknown_type_exp s

  fun unwrapUpd upd a =
    let
      fun unwrapLinrat (ComponentData.LinratAngleExpr e) = e
        | unwrapLinrat _ = raise unexpected_data_exp
    in
      unwrapLinrat (upd (ComponentData.LinratAngleExpr a))
    end;

  fun update upd (VData.Xnd a) = (VData.Xnd (unwrapUpd upd a))
    | update upd (VData.Znd a) = (VData.Znd (unwrapUpd upd a))
    | update _   (VData.Hnd)   = VData.Hnd;
end;

structure Test_OVertexComponentData_Param = OVertexComponentData_Param(
  structure VertexDataParam = Test_VertexComponentData_Param
  structure OVData = Test_RG_OVData);
structure Test_OVertexComponentData = ElementComponentData(Test_OVertexComponentData_Param);

