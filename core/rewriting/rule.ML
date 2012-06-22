(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*   Rules for rewriting graphs *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *)
(* 
    We store the lhs, rhs, lhs-automorphisms, and the computed rule-automorphisms. 
*)

signature RULE_SHARING =
sig
  structure Graph : OGRAPH_SHARING
  structure GraphIso : GRAPH_ISO_SHARING 
  sharing type GraphIso.graph = Graph.T
  sharing type GraphIso.subst = Graph.Param.subst 
  type T
end

signature BASIC_RULE 
= sig
  structure Graph : BANG_GRAPH
  structure GraphIso : BANG_GRAPH_ISO
  
  type T (* rule *)

  structure Sharing : RULE_SHARING
  sharing Sharing.Graph = Graph.SharingOGraph
  sharing Sharing.GraphIso = GraphIso.Sharing
  sharing type Sharing.T = T  

  exception bad_rule_exp of string * Graph.T * Graph.T;
  val mk : (Graph.T * Graph.T) -> T;
    
  (* NOTE/WARNING: these don't touch the automorphism data; they should not 
     be used without reconstructing the automoprhism info. *)
  val try_rename1_ename : E.name -> E.name -> T -> T option
  val try_rename1_vname : V.name -> V.name 
      -> T -> T option
  val try_rename1_bname : B.name -> B.name -> T -> T option
  val try_rename1_xname : X.name -> X.name -> T -> T option
  
  structure XCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = X.Rnm.T
  structure VertexCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = V.Rnm.T
  structure EdgeCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = E.Rnm.T
  structure BBoxCRnm : COMPOUND_RENAMING 
  where type obj = T and type renaming = B.Rnm.T

  val rename : (X.Rnm.T * V.Rnm.T * E.Rnm.T * B.Rnm.T) -> T 
            -> (X.Rnm.T * V.Rnm.T * E.Rnm.T * B.Rnm.T) * T

  val get_lhs : T -> Graph.T
  val get_rhs : T -> Graph.T
  
  val get_xnames : T -> X.NSet.T
  val get_vnames : T -> V.NSet.T
  val get_enames : T -> E.NSet.T
  val get_bnames : T -> B.NSet.T

  (* swaps lhs and rhs *)
  val symmetric : T -> T
  
  (* rule is a trivial identity : lhs matches rhs *)
  val is_trivial : T -> bool
  
  (* extra cached meta-data *)
  datatype 'a tree = Tree of ('a * 'a tree list);
  datatype constr   =   Constrained
                      | Arbitrary
  datatype 'a otype =   Initial of constr
                      | Rooted of constr * 'a
                      | Boundary of 'a
                      | Boxed of B.name * 'a otype;
  (* gets cached symmetry match-search order *)  
  val get_order : T -> (V.name otype * V.name tree) list
  (* gets self-applications *)
  val get_selfapps : T -> (VInjEndo.T * GraphIso.subst) list
  
  (* for pretty printing *)
  val pretty : T -> Pretty.T
  val print : T -> unit
end;

signature RULE 
= sig
  include BASIC_RULE
  (* TODO add stuff specific to automorphisms of rules *)
end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor Rule(Graph : BANG_GRAPH)
= struct

structure Graph = Graph;
structure GraphIso = BangGraphIso(Graph);

datatype 'a tree = Tree of ('a * 'a tree list);
type order = (V.name option * V.name tree) list;
datatype constr   =   Constrained
                    | Arbitrary
datatype 'a otype =   Initial of constr
                    | Rooted of constr * 'a
                    | Boundary of 'a
                    | Boxed of B.name * 'a otype;

fun keep_if_fixed n a = if V.name_eq ((  VInjEndo.domf
                                            o GraphIso.get_vmap) a n, n)
                        then [a]
                        else [];
fun fix v auts = maps (keep_if_fixed v) auts;

fun take_orbit auts vset =
    let val v   = (the o V.NSet.get_local_bot) vset
        val orb = fold (fn x => V.NSet.add ((  VInjEndo.domf
                                                  o GraphIso.get_vmap) x v))
                       auts V.NSet.empty
    in  (v, orb, V.NSet.subtract vset orb) end;

fun gen_order_sub g auts vset =
    if V.NSet.is_empty vset then (auts,[])
    else let val (v,orb,rest) = take_orbit auts vset
             val auts' = fix v auts
             val (auts'',subtr) = gen_order_sub g auts'
                                                (V.NSet.delete v orb)
             val (auts''',tail) = gen_order_sub g auts''
                                                (V.NSet.subtract vset orb)
         in  (auts''', Tree (v, subtr)::tail) end;
                           
fun gen_order g auts done bound =
    let fun maybe_box vn v = case Graph.lookup_bbox_of g vn of
                               NONE   => v
                             | SOME b => Boxed (b,v)
        val aset = Graph.incident_vertices g done
        val (root,vset)=if V.NSet.is_empty aset
                      then (NONE,Graph.get_vnames g
                                 |> V.NSet.remove_set done
                                 |> V.NSet.remove_set (Graph.get_boundary g))
                      else (SOME((the (* must be nonempty by adjacency *)
                                 o V.NSet.get_local_bot
                                oo V.NSet.intersect) done
                                     ((Graph.adj_vnames g
                                     o the
                                     o V.NSet.get_local_bot) aset))
                             ,aset)
    in if V.NSet.is_empty vset then []
    else case root of NONE => let val vsetb = V.NSet.intersect vset bound
                                  val (vset,typ) = if V.NSet.is_empty vsetb
                                                 then (vset,Initial Arbitrary)
                                                 else (vsetb,Initial Constrained)
                                  val (v,orb,_) = take_orbit auts vset
                                  val auts' = fix v auts
                                  val bound' = V.NSet.union_merge bound orb
                              in   (maybe_box v typ,Tree(v,[]))
                                 ::gen_order g auts'
                                             (V.NSet.add v done) bound' 
                              end
                  | SOME x => let val (v,orb,rest) = take_orbit auts vset
                                  val auts' = fix v auts
                                  val (auts'',subtr)
                                      = gen_order_sub g auts'
                                                      (V.NSet.delete v orb) 
                              in (maybe_box v 
                                   (if      V.NSet.contains bound v
                                  then Rooted (Constrained,x)
                                  else   if Graph.is_boundary g v
                                       then Boundary x
                                       else Rooted (Arbitrary,x)),
                                  Tree(v, subtr))::gen_order g auts''
                                         (V.NSet.union_merge done orb)
                                         bound
                              end
    end;

(* selfapps only contains a remapping of boundary vertices and substitution
   for data. *)
datatype T = Rule of { lhs : Graph.T,
                       rhs : Graph.T,
                       lhs_aut : GraphIso.T list,
                       selfapps : (VInjEndo.T * GraphIso.subst) list,
                       order : (V.name otype * V.name tree) list
                     };

exception bad_rule_exp of string * Graph.T * Graph.T;

(* Internal function that makes sure that:
  *   - Boundary names are the same
  *   - !-Boxes names are the same
  *   -Check if !-boxes in LHS bang the same boundary vertices as in
  *   RHS *)
fun check_rule_consistency lhs rhs = 
    let
      val lhsboundary = Graph.get_boundary lhs 
      val rhsboundary = Graph.get_boundary rhs
      val lhsboundary' = filter (Graph.is_bboxed lhs) (V.NSet.list_of lhsboundary)
      fun has_same_bangbox vn b = b andalso
          (Graph.is_bboxed rhs vn)   andalso
           B.name_eq ((Graph.get_bbox_of lhs vn),(Graph.get_bbox_of rhs vn)) 
    in
      (V.NSet.eq lhsboundary rhsboundary) andalso
      (B.NSet.eq (Graph.get_bbnames lhs) (Graph.get_bbnames rhs)) andalso
      (fold (has_same_bangbox) (lhsboundary') true)
   end; 

fun mk (lhs,rhs) = 
    let 
      val lhsbndry : V.NSet.T = Graph.get_boundary lhs
    in
    (* check that rule is well-formed! *)
    if check_rule_consistency lhs rhs
    then 
      let (* rename rhs; sharing bvertex names with lhs, 
             all else must be distinct, but do minimal amount of renaming *)
        val ignorevnames = (* ignore boundary and also fresh rhs vnames *)
            V.NSet.union_merge 
              lhsbndry
              (V.NSet.subtract (Graph.get_vnames rhs)
                                    (Graph.get_vnames lhs)); 
        (* ignore enames that are already fresh w.r.t. lhs *)
        val ignoreenames = E.NSet.subtract (Graph.get_enames rhs)
                            (Graph.get_enames lhs);
        val vrn = V.mk_renaming ignorevnames 
                    (V.NSet.union_merge ignorevnames 
                      (Graph.get_vnames lhs))
                    V.NTab.empty
        val ern = E.mk_renaming ignoreenames 
                    (E.NSet.union_merge ignoreenames (Graph.get_enames lhs))
                    E.NTab.empty
        val xrn = X.Rnm.empty;
        val (rn, rhs') = Graph.rename (xrn,vrn,ern) rhs
        val lhsauts = (Seq.list_of oo GraphIso.get) lhs lhs
        val rhsauts = (Seq.list_of oo GraphIso.get) rhs rhs'
        val lhsba = map (fn x => ((  VInjEndo.restrict_dom_to lhsbndry
                                   o GraphIso.get_vmap) x,GraphIso.get_subst x))
                        lhsauts
        val rhsba = map (fn x => ((  VInjEndo.restrict_dom_to lhsbndry
                                   o GraphIso.get_vmap) x,GraphIso.get_subst x))
                        rhsauts
        (* fetches one representative from the orbit of each LHS aut|boundary
           under Aut(rhs), using fact that l1~=l2 <-> there exists an r such
           that r.l2 = l1 *)
        fun filterorbits (x::xs) os =
            let val orbit = map ((VInjEndo.compose (fst x)) o fst) rhsba 
                fun eq a b = ((V.NTab.fold 
                                      (fn (d,c) => VInjEndo.add d c)
                                      (VInjEndo.get_domtab a) b; true)
                             handle VInjEndo.add_exp _ => false)
                val xs' = filter_out
                            (fn a => exists (eq (fst a)) orbit)
                            xs
            in filterorbits xs' (x::os) end
          | filterorbits [] os = os
      in Rule { lhs = lhs, rhs = rhs',
                lhs_aut = lhsauts,
                selfapps = filterorbits lhsba [],
                order = gen_order lhs lhsauts
                                  V.NSet.empty V.NSet.empty } end
    else
      raise bad_rule_exp ("mk: Left and right hand side boundaries are different", lhs, rhs)
    end;

(* 
fun rename_interior_vs xvs (avoidvs, avoides, avoidbs) g = 
     let (* rename rhs; shared bvertex names, all else is distinct. *)
        val vrn = V.mk_renaming xvs avoidvs V.NTab.empty
  in Graph.rename (vrn,E.Rnm.empty) g  end;
*)

(* match anything = no set matching *)
(* val any_match = Match.empty; *)

fun get_lhs (Rule rep) = #lhs rep;
fun get_rhs (Rule rep) = #rhs rep;
fun get_order (Rule rep) = #order rep;
fun get_selfapps (Rule rep) = #selfapps rep;

fun update_lhs f (Rule rep) = 
    Rule { lhs = f (#lhs rep), rhs = #rhs rep, lhs_aut = #lhs_aut rep, selfapps = #selfapps rep, order = #order rep };
fun update_rhs f (Rule rep) = 
    Rule { lhs = #lhs rep, rhs = f (#rhs rep), lhs_aut = #lhs_aut rep, selfapps = #selfapps rep, order = #order rep };

val set_lhs = update_lhs o K;
val set_rhs = update_rhs o K;

(* THINK: cache this info? *)
(* 
fun get_rhs_newnodes r = 
    let val (lhsns,rhsns) = Graph.get_vnames (get_rhs r)
    in V.NSet.subtract rhsns lhsns end;
fun get_lhs_delnodes r =
    let val (lhsns,rhsns) = Graph.get_vnames (get_lhs r)
    in V.NSet.subtract lhsns rhsns  end;
fun get_rhs_newedges r =
    let val (lhsns,rhsns) = Graph.get_enames (get_rhs r)
    in E.NSet.subtract rhsns lhsns end;
fun get_lhs_deledges r =
    let val (lhsns,rhsns) = Graph.get_enames (get_lhs r)
    in E.NSet.subtract lhsns rhsns  end;
*)

(* swap lhs and rhs of a rule *)
fun symmetric r = mk (get_lhs r, get_rhs r);

(* a rule is trivially true if the left and right hand sides match.
   Proof is by observing that bounary nodes can only be matched by
   boundary nodes and the types of the lhs and rhs must be equal to be
   a rule. *)
fun is_trivial rule = raise ERROR "not implemented"; (* FIXME *)
(* 
    let val init_renaming = 
        val rhs' = Graph.rename_apart
    in
    (case Seq.pull (MatchSearch.match (get_lhs rule) (get_rhs rule) 
                                     V.NTab.empty)
      of NONE => false
       | _ => true);
*)

(*  *)
(* TODO: add renaming for extra stuff (not lhs and rhs) *)
fun get_xnames rule = 
    X.NSet.union_merge 
      (Graph.get_xnames (get_lhs rule))
      (Graph.get_xnames (get_rhs rule));
fun get_vnames rule = 
    V.NSet.union_merge 
      (Graph.get_vnames (get_lhs rule))
      (Graph.get_vnames (get_rhs rule));
fun get_enames rule = 
    E.NSet.union_merge 
      (Graph.get_enames (get_lhs rule))
      (Graph.get_enames (get_rhs rule));
fun get_bnames rule = 
    B.NSet.union_merge 
      (Graph.get_bbnames (get_lhs rule))
      (Graph.get_bbnames (get_rhs rule));
    
(* *)
fun try_rename1_xname n1 n2 rule =
    (SOME 
      (rule |> (update_lhs (untryify (Graph.try_rename1_xname n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_xname n1 n2)))))
    handle tryify_failed () => NONE;
fun try_rename1_vname n1 n2 rule =
    (SOME 
      (rule |> (update_lhs (untryify (Graph.try_rename1_vname n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_vname n1 n2)))))
    handle tryify_failed () => NONE;
fun try_rename1_ename n1 n2 rule =     
    (SOME 
      (rule |> (update_lhs (untryify (Graph.try_rename1_ename n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_ename n1 n2)))))
    handle tryify_failed () => NONE;
fun try_rename1_bname n1 n2 rule = 
    (SOME 
      (rule |> (update_lhs (untryify (Graph.try_rename1_bname n1 n2)))
            |> (update_rhs (untryify (Graph.try_rename1_bname n1 n2)))))
    handle tryify_failed () => NONE;
    
(* *)
structure XCRnm = CompoundRenamingFun(
  struct 
    structure Nm = X;
    type obj = T
    val try_rename1 = try_rename1_xname;
    val get_nameset = get_xnames
  end);
structure VertexCRnm = CompoundRenamingFun(
  struct 
    structure Nm = V;
    type obj = T
    val try_rename1 = try_rename1_vname;
    val get_nameset = get_vnames
  end);
structure EdgeCRnm = CompoundRenamingFun(
  struct 
    structure Nm = E;
    type obj = T
    val try_rename1 = try_rename1_ename;
    val get_nameset = get_enames
  end);
structure BBoxCRnm = CompoundRenamingFun(
  struct 
    structure Nm = B;
    type obj = T
    val try_rename1 = try_rename1_bname;
    val get_nameset = get_bnames
  end);

(* rename *)
fun rename (xrn,vrn,ern,brn) rule = 
    let 
      val (xrn,rule) = XCRnm.rename xrn rule
      val (vrn,rule) = VertexCRnm.rename vrn rule
      val (ern,rule) = EdgeCRnm.rename ern rule
      val (brn,rule) = BBoxCRnm.rename brn rule
    in
      (* TODO: rename automorphism info; avoid re-creation. *)
      ((xrn,vrn,ern,brn), mk (get_lhs rule,get_rhs rule))
    end;

fun pretty (Rule rep) = 
    Pretty.chunks 
    [Pretty.block [Pretty.str "Rule: "],
     Pretty.block [Pretty.str "LHS: ", Graph.pretty (#lhs rep)],
     Pretty.block [Pretty.str "RHS: ", Graph.pretty (#rhs rep)]];

val print = Pretty.writeln o pretty;


structure Sharing = struct
  structure Graph = Graph.SharingOGraph;
  structure GraphIso = GraphIso.Sharing;
  type T = T;
end;

end;
