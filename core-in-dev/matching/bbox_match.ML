(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Data for !-Box matching: *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* information about bang-box matching in a graph *)
signature BBOX_MATCH_SHARING = 
sig
  structure G : BANG_GRAPH_SHARING
  structure BBMap : NAME_MAP_SHARING
  type T

  sharing BBMap.Dom = G.BBox;
  sharing BBMap.Cod = G.BBox;

end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BBOX_MATCH 
= sig

type T (* bang box match *)

structure G : BANG_GRAPH
structure BBMap : NAME_MAP

structure Sharing : BBOX_MATCH_SHARING
sharing type Sharing.T = T
sharing Sharing.BBMap = BBMap.Sharing
sharing Sharing.G = G.SharingBangGraph
(* bellow not needed; done by Sharing's sharings *)
sharing BBMap.Dom.Sharing = G.BBox.Sharing;
sharing BBMap.Cod.Sharing = G.BBox.Sharing;

val empty : T;
val init_with_exterior : G.Vertex.NSet.T -> T;
val init_from_pat : G.T -> T;
val mk_id : G.Vertex.NSet.T -> G.BBox.NSet.T -> T;

val get_bbmap : T -> BBMap.T;
val get_kill : T -> G.BBox.NSet.T
val get_drop : T -> G.BBox.NSet.T
val get_copies : T -> BBMap.T (* bbox copies *)
val get_crnm : T -> (G.Vertex.name G.Vertex.NTab.T (* copie of all vs and es*)
                     * G.Edge.name G.Edge.NTab.T) G.BBox.NTab.T
val get_xmaps : T -> G.Vertex.InjEndo.T G.BBox.NTab.T (* copies of exterior vertices *)
val get_xvs : T -> G.Vertex.NSet.T (* exterior vertices of pat *)

val update_bbmap : (BBMap.T -> BBMap.T) -> T -> T;


val add_kills : G.BBox.name list -> T -> T;
val add_drop : G.BBox.name -> T -> T;
val add_bbmap : G.BBox.name -> G.BBox.name -> T -> T;
val add_copy : (G.BBox.name (* old bbname *)
                 * (G.Vertex.name G.Vertex.NTab.T (* all vertex copying *)
                    * G.Edge.name G.Edge.NTab.T) (* edge copying *)
                 * G.BBox.name) (* new bbox name *) 
                -> T -> T;

val pretty : T -> Pretty.T
val print : T -> unit


end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor BBoxMatchFun(
  G : BANG_GRAPH)
: BBOX_MATCH
= struct

structure G = G;
structure V = G.OVertex;
structure E = G.Edge;
structure BBox = G.BBox;
structure BBMap = NameMapFun(structure Dom = BBox and Cod = BBox);

(* Note merging of bboxes is implicit from the map. 
   a match is 1. copy bboxes, kill some bboxes, drop some, and finally 
   map to tgt what's left. 
*)
datatype T = BBoxMatch of
         {
          copies : BBMap.T, (* 1. copy pattern bboxes to make extended pat *)
          kill : BBox.NSet.T, (* 2. kill boxes we don't map *)
          drop : BBox.NSet.T, (* 3. drop bboxes as needed *)
          bbmap : BBMap.T, (* 4. mapping of bboxes in extended pat to tgt *)
          (* how vertices get renamed when bboxes are copied - 
             for consistant naming; from new bbox names to renamings *)
          crnm : (V.name V.NTab.T * E.name E.NTab.T) BBox.NTab.T,
          (* external vertices in bang boxes that get copied/renamed; 
             these have to be the same between lhs and rhs of a rule 
             when bboxes are matched/instantiated. subset of those in crnm. *)
          xmaps : G.Vertex.InjEndo.T BBox.NTab.T,
          xvs : V.NSet.T (* all exterior vertices of !-expanded pat *)
         };

val empty = 
    BBoxMatch
      {bbmap = BBMap.empty,
       copies = BBMap.empty,
       kill = BBox.NSet.empty,
       drop = BBox.NSet.empty,
       crnm = BBox.NTab.empty,
       xmaps = BBox.NTab.empty,
       xvs = V.NSet.empty
      };

fun get_bbmap (BBoxMatch rep) = #bbmap rep;
fun get_copies (BBoxMatch rep) = #copies rep;
fun get_kill (BBoxMatch rep) = #kill rep;
fun get_drop (BBoxMatch rep) = #drop rep;
fun get_crnm (BBoxMatch rep) = #crnm rep;
fun get_xmaps (BBoxMatch rep) = #xmaps rep;
fun get_xvs (BBoxMatch rep) = #xvs rep;

fun update_bbmap f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = f (#bbmap rep),
       copies = #copies rep,
       kill = #kill rep,
       drop = #drop rep,
       crnm = #crnm rep,
       xmaps = #xmaps rep,
       xvs = #xvs rep
      };
fun update_copies f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = f (#copies rep),
       kill = #kill rep,
       drop = #drop rep,
       crnm = #crnm rep,
       xmaps = #xmaps rep,
       xvs = #xvs rep
      };
fun update_kill f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = #copies rep,
       kill = f (#kill rep),
       drop = #drop rep,
       crnm = #crnm rep,
       xmaps = #xmaps rep,
       xvs = #xvs rep
      };
fun update_drop f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = #copies rep,
       kill = #kill rep,
       drop = f (#drop rep),
       crnm = #crnm rep,
       xmaps = #xmaps rep,
       xvs = #xvs rep
      };
fun update_crnm f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = #copies rep,
       kill = #kill rep,
       drop = #drop rep,
       crnm = f (#crnm rep),
       xmaps = #xmaps rep,
       xvs = #xvs rep
      };      
fun update_xmaps f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = #copies rep,
       kill = #kill rep,
       drop = #drop rep,
       crnm = #crnm rep,
       xmaps = f (#xmaps rep),
       xvs = #xvs rep
      };
fun update_xvs f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = #copies rep,
       kill = #kill rep,
       drop = #drop rep,
       crnm = #crnm rep,
       xmaps = #xmaps rep,
       xvs = f (#xvs rep)
      };

val set_bbmap = update_bbmap o K;
val set_copies = update_copies o K;
val set_kill = update_kill o K;
val set_drop = update_drop o K;
val set_xmaps = update_xmaps o K;
val set_xvs = update_xvs o K;

fun mk_id xvs ns = 
    empty |> set_bbmap
          (BBox.NSet.fold 
             (fn n => fn bbm => BBMap.add_to_dom n n bbm)
             ns BBMap.empty)
          |> set_xvs xvs;

fun init_with_exterior xvs = 
    empty |> set_xvs xvs;

fun init_from_pat g = 
    empty |> set_xvs (G.get_boundary g);



(* 
val copy = update_drop o BBox.NSet.add;
*)

fun pretty_1crnm (vrn, ern) = 
    Pretty.chunks
    [ Pretty.block [Pretty.str "V Renaming: {", 
                    V.NTab.pretty V.pretty_name vrn, Pretty.str "}"],
      Pretty.block [Pretty.str "E Renaming: {", 
                    E.NTab.pretty E.pretty_name ern, Pretty.str "}"]
    ];

      
(* *)
fun pretty (BBoxMatch rep) = 
    Pretty.chunks
    [ Pretty.str "BBoxMap: {",
      Pretty.block [ Pretty.str "1. copies:", BBMap.pretty (#copies rep) ],
      Pretty.block [ Pretty.str "2. kill:", BBox.NSet.pretty (#kill rep) ],
      Pretty.block [ Pretty.str "3. drop:", BBox.NSet.pretty (#drop rep) ],
      Pretty.block [ Pretty.str "4. bbmap:", BBMap.pretty (#bbmap rep) ],
      Pretty.block [ Pretty.str "[xvs:", 
                     V.NSet.pretty (#xvs rep), 
                     Pretty.str "]" ],
      Pretty.block [ Pretty.str "[xmap:", 
                     BBox.NTab.pretty G.Vertex.InjEndo.pretty (#xmaps rep), 
                     Pretty.str "]" ],
      Pretty.block [ Pretty.str "[crnm:", 
                     BBox.NTab.pretty pretty_1crnm (#crnm rep), 
                     Pretty.str "]" ],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;


(* implicit args: ns m *)
val add_bbmap = update_bbmap oo BBMap.add_to_dom;
val add_kills = update_kill o fold BBox.NSet.add;
val add_drop = update_drop o BBox.NSet.add;
fun add_copy (bbn1,(vtab,etab),bbn2) m =
    let val xvs = get_xvs m 
        val xmaps = get_xmaps m
        val xmap = (case BBox.NTab.lookup xmaps bbn2 of NONE => G.Vertex.InjEndo.empty
                      | SOME xmap => xmap); 
        val (xvs2,xmap2) = 
              V.NTab.fold 
                (fn (n1,n2) => fn (x as (xvs,xmap)) => 
                 if V.NSet.contains xvs n1
                 then (V.NSet.add n2 xvs, G.Vertex.InjEndo.add n1 n2 xmap) else x)
                vtab (xvs,xmap);
    in
      m |> update_copies (BBMap.add_to_dom bbn2 bbn1)
        |> update_crnm (BBox.NTab.update (bbn2,(vtab,etab)))
        |> set_xvs xvs2 
        |> update_xmaps (BBox.NTab.update (bbn2,xmap2))
    end
    handle (ex as V.NSet.duplicate_exp (n,s)) => 
      (writeln "BUG: V.NSet.duplicate_exp"; V.print_name n; V.NSet.print s; print m; V.NTab.print V.pretty_name vtab; 
       E.NTab.print E.pretty_name etab;
       BBox.print_name bbn1; BBox.print_name bbn2;        
       raise ex);


structure Sharing = 
struct
  type T = T
  structure G = G.SharingBangGraph
  structure BBMap = BBMap.Sharing
end;


end;

