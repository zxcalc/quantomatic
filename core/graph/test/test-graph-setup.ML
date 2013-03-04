(* Graph structures for testing *)

structure Test_Graph_Data =
struct
  datatype vdata = VUnit1
                 | VUnit2
                 | VExpr1 of LinratAngleExpr.T
                 | VExpr2 of LinratAngleExpr.T
                 | VString1 of string
                 | VString2 of string;
  datatype edata = EUnit1
                 | EUnit2
                 | EExpr1 of LinratAngleExpr.T
                 | EExpr2 of LinratAngleExpr.T
                 | EString1 of string
                 | EString2 of string;
end;

structure Test_Graph_VData : UNIFIABLE_DATA where type data = Test_Graph_Data.vdata
= struct
  exception unimplemented_exp of string;

  open Test_Graph_Data;
  type data = vdata; 

  fun data_ord (VUnit1, VUnit1) = EQUAL
    | data_ord (VUnit1, _) = LESS
    | data_ord (_, VUnit1) = GREATER
    | data_ord (VUnit2, VUnit2) = EQUAL
    | data_ord (VUnit2, _) = LESS
    | data_ord (_, VUnit2) = GREATER
    | data_ord (VExpr1 a, VExpr1 b) = LinratAngleExpr.ord (a,b)
    | data_ord (VExpr1 _, _) = LESS
    | data_ord (_, VExpr1 _) = GREATER
    | data_ord (VExpr2 a, VExpr2 b) = LinratAngleExpr.ord (a,b)
    | data_ord (VExpr2 _, _) = LESS
    | data_ord (_, VExpr2 _) = GREATER
    | data_ord (VString1 a, VString1 b) = String.compare (a,b)
    | data_ord (VString1 _, _) = LESS
    | data_ord (_, VString1 _) = GREATER
    | data_ord (VString2 a, VString2 b) = String.compare (a,b);
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
      
  fun pretty_data VUnit1 = Pretty.str "VUnit1"
    | pretty_data VUnit2 = Pretty.str "VUnit2"
    | pretty_data (VExpr1 a) = 
          Pretty.block [
            Pretty.str "VExpr1(", LinratAngleExpr.pretty a, Pretty.str ")"
          ]
    | pretty_data (VExpr2 a) = 
          Pretty.block [
            Pretty.str "VExpr2(", LinratAngleExpr.pretty a, Pretty.str ")"
          ]
    | pretty_data (VString1 s) = 
          Pretty.block [
            Pretty.str "VString1(", Pretty.str s, Pretty.str ")"
          ]
    | pretty_data (VString2 s) = 
          Pretty.block [
            Pretty.str "VString2(", Pretty.str s, Pretty.str ")"
          ];
  val print_data = Pretty.writeln o pretty_data;
  
  type subst = LinratAngleExpr.match;
  val empty_subst = LinratAngleExpr.empty_match;
  fun compose_subst (u1,u2) = LinratAngleExpr.compose_matches u1 u2;

  fun match_data ((VExpr1 a1),(VExpr1 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_data ((VExpr2 a1),(VExpr2 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_data (v1,v2) m = 
        if data_eq (v1, v2) then SOME m else NONE; 
  fun unify_data _ _ = 
        raise unimplemented_exp "Test_Graph_VData.unify_data";
  fun subst_in_data u (VExpr1 a) = VExpr1 (LinratAngleExpr.subst u a)
    | subst_in_data u (VExpr2 a) = VExpr2 (LinratAngleExpr.subst u a)
    | subst_in_data _ v = v;
    
  val pretty_subst = LinratAngleExpr.pretty_match;
  val print_subst = Pretty.writeln o pretty_subst;
    
  val default_data = VUnit1;
  
  structure Sharing = struct type data = data; type subst = subst; end
end;

structure Test_Graph_EData : UNIFIABLE_DATA where type data = Test_Graph_Data.edata
= struct
  exception unimplemented_exp of string;

  open Test_Graph_Data;
  type data = edata; 

  fun data_ord (EUnit1, EUnit1) = EQUAL
    | data_ord (EUnit1, _) = LESS
    | data_ord (_, EUnit1) = GREATER
    | data_ord (EUnit2, EUnit2) = EQUAL
    | data_ord (EUnit2, _) = LESS
    | data_ord (_, EUnit2) = GREATER
    | data_ord (EExpr1 a, EExpr1 b) = LinratAngleExpr.ord (a,b)
    | data_ord (EExpr1 _, _) = LESS
    | data_ord (_, EExpr1 _) = GREATER
    | data_ord (EExpr2 a, EExpr2 b) = LinratAngleExpr.ord (a,b)
    | data_ord (EExpr2 _, _) = LESS
    | data_ord (_, EExpr2 _) = GREATER
    | data_ord (EString1 a, EString1 b) = String.compare (a,b)
    | data_ord (EString1 _, _) = LESS
    | data_ord (_, EString1 _) = GREATER
    | data_ord (EString2 a, EString2 b) = String.compare (a,b);
  fun data_eq (n1, n2) = case data_ord (n1, n2) of EQUAL => true | _ => false;
      
  fun pretty_data EUnit1 = Pretty.str "EUnit1"
    | pretty_data EUnit2 = Pretty.str "EUnit2"
    | pretty_data (EExpr1 a) = 
          Pretty.block [
            Pretty.str "EExpr1(", LinratAngleExpr.pretty a, Pretty.str ")"
          ]
    | pretty_data (EExpr2 a) = 
          Pretty.block [
            Pretty.str "EExpr2(", LinratAngleExpr.pretty a, Pretty.str ")"
          ]
    | pretty_data (EString1 s) = 
          Pretty.block [
            Pretty.str "EString1(", Pretty.str s, Pretty.str ")"
          ]
    | pretty_data (EString2 s) = 
          Pretty.block [
            Pretty.str "EString2(", Pretty.str s, Pretty.str ")"
          ];
  val print_data = Pretty.writeln o pretty_data;
  
  type subst = LinratAngleExpr.match;
  val empty_subst = LinratAngleExpr.empty_match;
  fun compose_subst (u1,u2) = LinratAngleExpr.compose_matches u1 u2;

  fun match_data ((EExpr1 a1),(EExpr1 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_data ((EExpr2 a1),(EExpr2 a2)) m = 
        LinratAngleExpr.match a1 a2 m
    | match_data (v1,v2) m = 
        if data_eq (v1, v2) then SOME m else NONE; 
  fun unify_data _ _ = 
        raise unimplemented_exp "Test_Graph_EData.unify_data";
  fun subst_in_data u (EExpr1 a) = EExpr1 (LinratAngleExpr.subst u a)
    | subst_in_data u (EExpr2 a) = EExpr2 (LinratAngleExpr.subst u a)
    | subst_in_data _ v = v;
    
  val pretty_subst = LinratAngleExpr.pretty_match;
  val print_subst = Pretty.writeln o pretty_subst;
    
  val default_data = EUnit1;
  
  structure Sharing = struct
    type data = data;
    type subst = subst;
  end;
end;

structure Test_GraphParam : GRAPH_PARAM
= struct
  structure VData = Test_Graph_VData;
  structure EData = Test_Graph_EData;

  structure VDep = VXBRel;
  structure EDep = EXBRel;
  type T = VDep.T * EDep.T;

  open Test_Graph_Data;

  fun pretty (vd,ed) = Pretty.block [VDep.pretty vd,EDep.pretty ed];
  val print = Pretty.writeln o pretty;

  exception match_exp of unit;
  exception unknown_type_exp of string;

  fun subst_xname_in_vdata n1 n2 (VExpr1 a) =
        (case LinratAngleExpr.try_rename1 n1 n2 a
           of SOME b => VExpr1 b
            | NONE   => raise X.Rnm.invalid_renaming_exp "subst_xname_in_vdata")
    | subst_xname_in_vdata n1 n2 (VExpr2 a) =
        (case LinratAngleExpr.try_rename1 n1 n2 a
           of SOME b => VExpr2 b
            | NONE   => raise X.Rnm.invalid_renaming_exp "subst_xname_in_vdata")
    | subst_xname_in_vdata _ _ d = d;
  fun subst_xname_in_edata n1 n2 (EExpr1 a) =
        (case LinratAngleExpr.try_rename1 n1 n2 a
           of SOME b => EExpr1 b
            | NONE   => raise X.Rnm.invalid_renaming_exp "subst_xname_in_edata")
    | subst_xname_in_edata n1 n2 (EExpr2 a) =
        (case LinratAngleExpr.try_rename1 n1 n2 a
           of SOME b => EExpr2 b
            | NONE   => raise X.Rnm.invalid_renaming_exp "subst_xname_in_edata")
    | subst_xname_in_edata _ _ d = d;
  
  fun get_xnames (vdep,edep) = X.NSet.union_merge (VDep.get_codset vdep) (EDep.get_codset edep);
  fun get_vnames (vdep,_) = VDep.get_domset vdep;
  fun get_enames (_,edep) = EDep.get_domset edep;

  fun dep_vnames_of_xname (vdep,_) n = VDep.codf vdep n;
  fun dep_enames_of_xname (_,edep) n = EDep.codf edep n;

  fun try_rename1_xname n1 n2 (vdep,edep) =
        case VDep.try_rename1_cod n1 n2 vdep
          of NONE       => NONE
           | SOME vdep' =>
               (case EDep.try_rename1_cod n1 n2 edep
                  of NONE       => NONE
                   | SOME edep' => SOME (vdep',edep'));
  fun try_rename1_vname n1 n2 (vdep,edep) =
        case VDep.try_rename1_dom n1 n2 vdep
          of SOME vdep' => SOME (vdep',edep)
           | NONE       => NONE;
  fun try_rename1_ename n1 n2 (vdep,edep) =
        case EDep.try_rename1_dom n1 n2 edep
          of SOME edep' => SOME (vdep,edep')
           | NONE       => NONE;

  fun rename_xname n1 n2 (vdep,edep) = 
        case VDep.try_rename1_cod n1 n2 vdep
          of NONE       => 
               raise V.Rnm.invalid_renaming_exp "Test_GraphParam.rename_xname"
           | SOME vdep' =>
               (case EDep.try_rename1_cod n1 n2 edep
                  of NONE       =>
                       raise E.Rnm.invalid_renaming_exp "Test_GraphParam.rename_xname"
                   | SOME edep' => (vdep',edep'));
  fun rename_vname n1 n2 (vdep,edep) = 
        case VDep.try_rename1_dom n1 n2 vdep
          of SOME vdep' => (vdep',edep)
           | NONE       =>
               raise V.Rnm.invalid_renaming_exp "Test_GraphParam.rename_vname";
  fun rename_ename n1 n2 (vdep,edep) =
        case EDep.try_rename1_dom n1 n2 edep
          of SOME edep' => (vdep,edep')
           | NONE       => 
               raise E.Rnm.invalid_renaming_exp "Test_GraphParam.rename_ename";

  (* rename names in T *)

  (* rename internal names in the dependency structure *)
  structure CRnmX : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = X
           type obj = T
           val get_nameset = get_xnames
           val try_rename1 = try_rename1_xname
    end);

  (* rename vertices in the dependency structure *)
  structure CRnmV : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = V
           type obj = T
           val get_nameset = get_vnames
           val try_rename1 = try_rename1_vname
    end);

  (* rename vertices in the dependency structure *)
  structure CRnmE : COMPOUND_RENAMING
  = CompoundRenamingFun(
    struct structure Nm = E
           type obj = T
           val get_nameset = get_enames
           val try_rename1 = try_rename1_ename
    end);

  (* empty name-dependency information *)
  val empty = (VDep.empty,EDep.empty);

  fun add_vertex vd vn (vdep,edep) = 
        case vd
          of VExpr1 a =>
               (VDep.add_to_dom vn (LinratAngleExpr.get_vars a) vdep,edep)
           | VExpr2 a =>
               (VDep.add_to_dom vn (LinratAngleExpr.get_vars a) vdep,edep)
           | _        => (vdep,edep);
  fun add_edge ed en (vdep,edep) = 
        case ed
          of EExpr1 a =>
               (vdep,EDep.add_to_dom en (LinratAngleExpr.get_vars a) edep)
           | EExpr2 a =>
               (vdep,EDep.add_to_dom en (LinratAngleExpr.get_vars a) edep)
           | _        => (vdep,edep);

  fun delete_vname vn (vdep,edep) = (VDep.del_dom vn vdep,edep);
  fun delete_ename en (vdep,edep) = (vdep,EDep.del_dom en edep);

  fun replace_vertex vd vn dep = 
        dep |> delete_vname vn
            |> add_vertex vd vn;
  fun replace_edge ed en dep = 
        dep |> delete_ename en
            |> add_edge ed en;

  fun of_vertex vd vn = add_vertex vd vn (VDep.empty,EDep.empty);
  fun of_edge ed en = add_edge ed en (VDep.empty,EDep.empty);
  
  (* substitutions for variables in vertex/edge data *)
  type subst = VData.subst; (* = EData.subst *)

  val empty_subst = VData.empty_subst;
  fun compose_subst u1 u2 = VData.compose_subst (u1,u2);

  fun try_match_vertex d1 d2 u = VData.match_data (d1,d2) u;
  fun try_match_edge   d1 d2 u = EData.match_data (d1,d2) u;

  (* raises match_exp *)
  fun match_vertex d1 d2 u = 
      (case try_match_vertex d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 
  
  fun match_edge d1 d2 u =
      (case try_match_edge d1 d2 u 
         of NONE    => raise match_exp ()
          | SOME u' => u'); 

  fun subst_in_vertex u vd = VData.subst_in_data u vd;
  fun subst_in_edge   u ed = EData.subst_in_data u ed;

  val pretty_subst = VData.pretty_subst;
  val print_subst = Pretty.writeln o pretty_subst;

  val pretty_theory_name = Pretty.str "test theory";

  structure Sharing = 
  struct 
    structure VData = VData.Sharing;
    structure EData = EData.Sharing;
    type T = T;
    type subst = subst;
  end;
end;

structure Test_Graph : GRAPH
= GraphFun(BasicGraphFun(
    structure Param = Test_GraphParam;
    type data = unit
    val empty_data = ()));

structure Test_OGraph : OGRAPH
= OGraphFun(BasicOGraphFun(
    structure Param = OGraphParam(
      structure Param = Test_GraphParam 
      and OVData = OVertexData(Test_Graph_VData)
    );
    type data = unit
    val empty_data = ()));

structure Test_Bang_Graph : BANG_GRAPH
= struct 
  structure BasicBangGraph = BasicBangGraphFun(
    structure Param = OGraphParam(
      structure Param = Test_GraphParam 
      and OVData = OVertexData(Test_Graph_VData)
    );
    type data = unit
    val empty_data = ());
  structure BBGraph = BangGraphTools(BasicBangGraph);
  open BBGraph;
end;

structure Test_Bang_Graph_IO =
struct
  structure ComponentDataIO (* : GRAPH_COMPONENT_DATA_IO *)
  = struct
    type vdata = Test_Graph_Data.vdata;
    type edata = Test_Graph_Data.edata;

    structure IVDataInputJSON : INPUT_JSON =
    struct
      open JsonInputUtils;
      open Test_Graph_Data;
      structure L = InputLinratJSON;
      type data = vdata;
      val get_angle = L.input oo (get_easy Json.Null)
      val to_lower = String.implode o (map Char.toLower) o String.explode
      fun input (Json.String t) =
            (case to_lower t
               of "vunit1" => VUnit1
                | "vunit2" => VUnit2
                | "vexpr1" => VExpr1 LinratAngleExpr.zero
                | "vexpr2" => VExpr2 LinratAngleExpr.zero
                | "vstring1" => VString1 ""
                | "vstring2" => VString2 ""
                | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
        | input (Json.Object obj) =
            (case to_lower (get_string obj "type")
               of "vunit1" => VUnit1
                | "vunit2" => VUnit2
                | "vexpr1" => VExpr1 (get_angle obj "angle")
                | "vexpr2" => VExpr2 (get_angle obj "angle")
                | "vstring1" => VString1 (get_string_easy "" obj "data")
                | "vstring2" => VString2 (get_string_easy "" obj "data")
                | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
        | input _ = raise bad_input_exp ("Expected object","");
    end;
    structure IVDataOutputJSON : OUTPUT_JSON =
    struct
      open JsonOutputUtils;
      open Test_Graph_Data;
      structure L = OutputLinratJSON;
      type data = vdata;
      fun output VUnit1 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VUnit1")
            )
        | output VUnit2 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VUnit2")
            )
        | output (VExpr1 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VExpr1")
                             |> update ("angle",L.output a)
            )
        | output (VExpr2 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VExpr2")
                             |> update ("angle",L.output a)
            )
        | output (VString1 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VString1")
                             |> update ("data",Json.String s)
            )
        | output (VString2 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "VString2")
                             |> update ("data",Json.String s)
            );
    end;
    structure EDataInputJSON : INPUT_JSON =
    struct
      open JsonInputUtils;
      open Test_Graph_Data;
      structure L = InputLinratJSON;
      type data = edata;
      val get_angle = L.input oo (get_easy Json.Null)
      val to_lower = String.implode o (map Char.toLower) o String.explode
      fun input (Json.String t) =
            (case to_lower t
               of "eunit1" => EUnit1
                | "eunit2" => EUnit2
                | "eexpr1" => EExpr1 LinratAngleExpr.zero
                | "eexpr2" => EExpr2 LinratAngleExpr.zero
                | "estring1" => EString1 ""
                | "estring2" => EString2 ""
                | _   => raise bad_input_exp ("Unknown vertex type "^t,""))
        | input (Json.Object obj) =
            (case to_lower (get_string obj "type")
               of "eunit1" => EUnit1
                | "eunit2" => EUnit2
                | "eexpr1" => EExpr1 (get_angle obj "angle")
                | "eexpr2" => EExpr2 (get_angle obj "angle")
                | "estring1" => EString1 (get_string_easy "" obj "data")
                | "estring2" => EString2 (get_string_easy "" obj "data")
                | t   => raise bad_input_exp ("Unknown vertex type "^t,"type"))
        | input _ = raise bad_input_exp ("Expected object","");
    end;
    structure EDataOutputJSON : OUTPUT_JSON =
    struct
      open JsonOutputUtils;
      open Test_Graph_Data;
      structure L = OutputLinratJSON;
      type data = edata;
      fun output EUnit1 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EUnit1")
            )
        | output EUnit2 =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EUnit2")
            )
        | output (EExpr1 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EExpr1")
                             |> update ("angle",L.output a)
            )
        | output (EExpr2 a) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EExpr2")
                             |> update ("angle",L.output a)
            )
        | output (EString1 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EString1")
                             |> update ("data",Json.String s)
            )
        | output (EString2 s) =
            Json.Object (
              Json.empty_obj |> update ("type",Json.String "EString2")
                             |> update ("data",Json.String s)
            );
    end;

    structure DotStyle : DOT_STYLE =
    struct
      open Test_Graph_Data;
      type vdata = vdata;
      fun style_for_ivertex_data VUnit1 =
            "[style=filled,fillcolor=white,fontcolor=black,shape=square]"
        | style_for_ivertex_data VUnit2 =
            "[style=filled,fillcolor=black,fontcolor=black,shape=square]"
        | style_for_ivertex_data (VExpr1 _) =
            "[style=filled,fillcolor=red,fontcolor=black,shape=circle]"
        | style_for_ivertex_data (VExpr2 _) =
            "[style=filled,fillcolor=green,fontcolor=black,shape=circle]"
        | style_for_ivertex_data (VString1 _) =
            "[style=filled,fillcolor=blue,fontcolor=black,shape=triangle]"
        | style_for_ivertex_data (VString2 _) =
            "[style=filled,fillcolor=pink,fontcolor=black,shape=triangle]";
    end;
  end;
  structure InputGraphJSON = InputGraphJSON(
    structure Graph = Test_Bang_Graph
    structure InputVertexData = ComponentDataIO.IVDataInputJSON
    structure InputEdgeData = ComponentDataIO.EDataInputJSON
  );
  structure OutputGraphJSON = OutputGraphJSON(
    structure Graph = Test_Bang_Graph
    structure OutputVertexData = ComponentDataIO.IVDataOutputJSON
    structure OutputEdgeData = ComponentDataIO.EDataOutputJSON
  );
  structure OutputGraphDot = OutputGraphDot(
    structure Graph = Test_Bang_Graph
    structure DotStyle = ComponentDataIO.DotStyle
  );
end;


functor Test_Graph_Tools(
  structure G : GRAPH where type EData.data = Test_Graph_Data.edata
  val vert : Test_Graph_Data.vdata -> G.VData.data
  val unwrap_vert : G.VData.data -> Test_Graph_Data.vdata
)
= struct
  structure G    = G;
  structure Data = Test_Graph_Data;
  structure GIso = GraphIsoFun(structure Graph = G);
  structure Expr = LinratAngleExpr;

  val vert = vert;
  val unwrap_vert = unwrap_vert;

  val vunit1     = vert Data.VUnit1;
  val vunit2     = vert Data.VUnit2;
  fun vexpr1   a = vert (Data.VExpr1 (Expr.parse a));
  val vexpr1_a   = vert o Data.VExpr1;
  fun vexpr2   a = vert (Data.VExpr2 (Expr.parse a));
  val vexpr2_a   = vert o Data.VExpr2;
  fun vstring1 s = vert (Data.VString1 s);
  fun vstring2 s = vert (Data.VString2 s);

  val eunit1     = Data.EUnit1;
  val eunit2     = Data.EUnit2;
  fun eexpr1   a = (Data.EExpr1 (Expr.parse a));
  val eexpr1_a   = Data.EExpr1;
  fun eexpr2   a = (Data.EExpr2 (Expr.parse a));
  val eexpr2_a   = Data.EExpr2;
  fun estring1 s = (Data.EString1 s);
  fun estring2 s = (Data.EString2 s);

  (* creating stuff *)
  fun add_vunit1   n   = G.add_named_vertex (V.mk n) vunit1;
  fun add_vunit2   n   = G.add_named_vertex (V.mk n) vunit2;
  fun add_vexpr1   n a = G.add_named_vertex (V.mk n) (vexpr1 a);
  fun add_vexpr1_a n a = G.add_named_vertex (V.mk n) (vexpr1_a a);
  fun add_vexpr2   n a = G.add_named_vertex (V.mk n) (vexpr2 a);
  fun add_vexpr2_a n a = G.add_named_vertex (V.mk n) (vexpr2_a a);
  fun add_vstring1 n s = G.add_named_vertex (V.mk n) (vstring1 s);
  fun add_vstring2 n s = G.add_named_vertex (V.mk n) (vstring2 s);

  fun add_dir_eunit1 n s t =
        G.add_named_edge (E.mk n) (G.Directed,eunit1) (V.mk s) (V.mk t);
  fun add_dir_eunit2 n s t =
        G.add_named_edge (E.mk n) (G.Directed,eunit2) (V.mk s) (V.mk t);
  fun add_dir_eexpr1 n s t a =
        G.add_named_edge (E.mk n) (G.Directed,(eexpr1 a)) (V.mk s) (V.mk t);
  fun add_dir_eexpr1_a n s t a =
        G.add_named_edge (E.mk n) (G.Directed,(eexpr1_a a)) (V.mk s) (V.mk t);
  fun add_dir_eexpr2 n s t a =
        G.add_named_edge (E.mk n) (G.Directed,(eexpr2 a)) (V.mk s) (V.mk t);
  fun add_dir_eexpr2_a n s t a =
        G.add_named_edge (E.mk n) (G.Directed,(eexpr2_a a)) (V.mk s) (V.mk t);
  fun add_dir_estring1 n str s t =
        G.add_named_edge (E.mk n) (G.Directed,(estring1 str)) (V.mk s) (V.mk t);
  fun add_dir_estring2 n str s t =
        G.add_named_edge (E.mk n) (G.Directed,(estring2 str)) (V.mk s) (V.mk t);

  fun add_undir_eunit1 n s t =
        G.add_named_edge (E.mk n) (G.UnDirected,Data.EUnit1) (V.mk s) (V.mk t);
  fun add_undir_eunit2 n s t =
        G.add_named_edge (E.mk n) (G.UnDirected,Data.EUnit2) (V.mk s) (V.mk t);
  fun add_undir_eexpr1 n s t a =
        G.add_named_edge (E.mk n) (G.UnDirected,(Data.EExpr1 (Expr.parse a))) (V.mk s) (V.mk t);
  fun add_undir_eexpr1_a n s t a =
        G.add_named_edge (E.mk n) (G.UnDirected,(Data.EExpr1 a)) (V.mk s) (V.mk t);
  fun add_undir_eexpr2 n s t a =
        G.add_named_edge (E.mk n) (G.UnDirected,(Data.EExpr2 (Expr.parse a))) (V.mk s) (V.mk t);
  fun add_undir_eexpr2_a n s t a =
        G.add_named_edge (E.mk n) (G.UnDirected,(Data.EExpr2 a)) (V.mk s) (V.mk t);
  fun add_undir_estring1 n str s t =
        G.add_named_edge (E.mk n) (G.UnDirected,(Data.EString1 str)) (V.mk s) (V.mk t);
  fun add_undir_estring2 n str s t =
        G.add_named_edge (E.mk n) (G.UnDirected,(Data.EString2 str)) (V.mk s) (V.mk t);

  (* testing stuff *)
  fun dir_eq G.Directed G.Directed = true
    | dir_eq G.UnDirected G.UnDirected = true
    | dir_eq _ _ = false;
  fun assert_vnames exp_vs actual_vset =
    let
      val exp_vset = V.NSet.of_list (map V.mk exp_vs);
      val pretty_set = Pretty.string_of o V.NSet.pretty
    in
      if V.NSet.eq exp_vset actual_vset
      then ()
      else raise ERROR ("assert_vnames: expected "^(pretty_set exp_vset)^
                        " but got "^(pretty_set actual_vset))
    end;
  fun assert_enames exp_es actual_eset =
    let
      val exp_ens = E.NSet.of_list (map E.mk exp_es);
      val pretty_set = Pretty.string_of o E.NSet.pretty
    in
      if E.NSet.eq exp_ens actual_eset
      then ()
      else raise ERROR ("assert_enames: expected "^(pretty_set exp_ens)^
                        " but got "^(pretty_set actual_eset))
    end;
  fun assert_xnames exp_vs actual_vset =
    let
      val exp_vset = X.NSet.of_list (map X.mk exp_vs);
      val pretty_set = Pretty.string_of o X.NSet.pretty
    in
      if X.NSet.eq exp_vset actual_vset
      then ()
      else raise ERROR ("assert_xnames: expected "^(pretty_set exp_vset)^
                        " but got "^(pretty_set actual_vset))
    end;
  fun assert_vertices exp_vs actual_vtab =
    let
      val _ = assert_vnames (map fst exp_vs) (V.NTab.get_nameset actual_vtab)
      fun check_verts ((vn,(vd,(ins,outs)))::vs) =
        let
          val (vd',(ins',outs')) = V.NTab.get actual_vtab (V.mk vn);
          val _ = Testing.assert ("same data for "^vn)
                    (G.VData.data_eq (vd,vd'));
          val _ = Testing.assert ("same ins for "^vn)
                    (E.NSet.eq ins' (E.NSet.of_list (map E.mk ins)))
          val _ = Testing.assert ("same outs for "^vn)
                    (E.NSet.eq outs' (E.NSet.of_list (map E.mk outs)))
        in check_verts vs end
        | check_verts [] = ();
    in check_verts exp_vs end;
  fun assert_edges exp_es actual_etab =
    let
      val _ = assert_enames (map fst exp_es) (E.NTab.get_nameset actual_etab)
      fun check_edges ((en,((dir,ed),(src,tgt)))::es) =
        let
          val ((dir',ed'),(src',tgt')) = E.NTab.get actual_etab (E.mk en);
          val _ = Testing.assert ("same dir for "^en)
                    (dir_eq dir dir');
          val _ = Testing.assert ("same data for "^en)
                    (G.EData.data_eq (ed,ed'));
          val _ = Testing.assert ("same source for "^en)
                    (V.name_eq ((V.mk src),src'));
          val _ = Testing.assert ("same target for "^en)
                    (V.name_eq ((V.mk tgt),tgt'));
        in check_edges es end
        | check_edges [] = ();
    in check_edges exp_es end;
  fun assert_none _ NONE = ()
    | assert_none name (SOME _) = raise ERROR (name^" is not NONE");

  (* FIXME: move into Testing in isaplib *)
  fun test_force s f v =
    case Testing.test s f v of
         NONE => raise Testing.failed_tests_exp (!Testing.tests_failed_ref)
       | SOME x => x
end;


(* Some helpful tools for creating string graphs *)
functor Test_OGraph_Tools(
  G : OGRAPH where type EData.data = Test_Graph_Data.edata
               and type OVData.IData.data = Test_Graph_Data.vdata
)
= struct
  structure OG = G;

  fun unwrap_nv G.OVData.WVert = raise ERROR "wire vertex"
    | unwrap_nv (G.OVData.NVert vd) = vd;
  val nv = G.OVData.NVert;
  fun add_wv n = G.add_named_vertex (V.mk n) G.OVData.WVert

  structure GraphTools = Test_Graph_Tools(
    structure G  = G
    val vert = nv
    val unwrap_vert = unwrap_nv
  );
  open GraphTools;

  structure G = OG;
end;


(* Some helpful tools for creating graphs *)
structure Test_Bang_Graph_Tools =
struct
  structure G = Test_Bang_Graph;

  fun unwrap_nv G.OVData.WVert = raise ERROR "wire vertex"
    | unwrap_nv (G.OVData.NVert vd) = vd;
  val nv = G.OVData.NVert;
  fun add_wv n = G.add_named_vertex (V.mk n) G.OVData.WVert

  structure GraphTools = Test_Graph_Tools(
    structure G  = G
    val vert = nv
    val unwrap_vert = unwrap_nv
  );
  open GraphTools;

  structure G = Test_Bang_Graph;
  structure GIso = BangGraphIso(G);
  structure GJsonInput = Test_Bang_Graph_IO.InputGraphJSON;
  structure GJsonOutput = Test_Bang_Graph_IO.OutputGraphJSON;
  structure GDotOutput = Test_Bang_Graph_IO.OutputGraphDot;

  fun add_to_bbox _ [] = I
    | add_to_bbox n vs =
        let
          val bn = B.mk n
          val vset = V.NSet.of_list (map V.mk vs)
        in
          G.doadd_to_bbox bn vset
        end;
  fun add_bbox n vs = (add_to_bbox n vs) o (G.add_named_bbox (B.mk n));
  fun add_child_bbox n parent vs =
          (G.set_bbox_parent (B.mk n) (B.mk parent)) o
          (add_to_bbox n vs) o
          (G.add_named_bbox (B.mk n))

  local
    fun write_to_file file s = let
      val outstream = TextIO.openOut file
    in
      (TextIO.output (outstream, s);
       TextIO.closeOut outstream)
    end;
  in
    fun graph_from_json str    = GJsonInput.input str;
    fun load_graph      path   = GJsonInput.input (Json.read_file path);
    fun save_dot_graph  g path = write_to_file path (GDotOutput.output g);

    val print_dot_graph        = writeln o GDotOutput.output;
    val print_json_graph       = Pretty.writeln o Json.pretty o GJsonOutput.output;
  end;

  fun num_bboxes g = B.NSet.cardinality (G.get_bboxes g)
  
  fun assert_n_bboxes g n () =
    if (num_bboxes g) = n
    then () else raise ERROR(
      "expected: " ^ Int.toString n ^ " bboxes, "^
      "got: " ^ Int.toString (num_bboxes g))
end;

