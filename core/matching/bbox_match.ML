(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Data for !-Box matching: *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* information about bang-box matching in a graph *)
signature BBOX_MATCH_SHARING = 
sig
  structure G : OGRAPH_SHARING
  type T
end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BBOX_MATCH 
= sig

type T (* bang box match *)

structure G : BANG_GRAPH

structure Sharing : BBOX_MATCH_SHARING
sharing type Sharing.T = T
sharing Sharing.G = G.SharingOGraph

val empty : T;
val init_with_exterior : V.NSet.T -> T;
val init_from_pat : G.T -> T;
val mk_id : V.NSet.T -> B.NSet.T -> T;

val get_bbmap : T -> BMap.T;
val get_kill : T -> B.NSet.T
val get_drop : T -> B.NSet.T
val get_copies : T -> BMap.T (* bbox copies *)
val get_crnm : T -> (V.name V.NTab.T (* copie of all vs and es*)
                     * E.name E.NTab.T) B.NTab.T
val get_xmaps : T -> VInjEndo.T B.NTab.T (* copies of exterior vertices *)
val get_xvs : T -> V.NSet.T (* exterior vertices of pat *)

val update_bbmap : (BMap.T -> BMap.T) -> T -> T;


val add_kills : B.name list -> T -> T;
val add_drop : B.name -> T -> T;
val add_bbmap : B.name -> B.name -> T -> T;
val add_copy : (B.name (* old bbname *)
                 * (V.name V.NTab.T (* all vertex copying *)
                    * E.name E.NTab.T) (* edge copying *)
                 * B.name) (* new bbox name *) 
                -> T -> T;

(* apply bbox expantion to matched graph (lhs), and some rhs of a rule 
   graph (rhs) *)
val inst_pat_bbox_match : T -> G.T -> G.T
val inst_replacement_bbox_match : 
      T -> (V.NSet.T * E.NSet.T) * G.T 
        -> (V.NSet.T * E.NSet.T) * G.T

val pretty : T -> Pretty.T
val print : T -> unit


end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor BBoxMatchFun(
  G : BANG_GRAPH)
: BBOX_MATCH
= struct

structure G = G;

(* Note merging of bboxes is implicit from the map. 
   a match is: copy bboxes, kill some bboxes, drop some, and finally 
   map to tgt what's left. 
*)
datatype T = BBoxMatch of
         {
          copies : BMap.T, (* 1. copy (new name => old name) *)
          kill : B.NSet.T, (* 2. kill boxes we don't map *)
          drop : B.NSet.T, (* 3. drop bboxes as needed *)
          bbmap : BMap.T, (* 4. mapping of bboxes in extended pat to tgt *)
          (* how vertices get renamed when bboxes are copied - 
             for consistant naming; from new bbox names to renamings *)
          crnm : (V.name V.NTab.T * E.name E.NTab.T) B.NTab.T,
          (* external vertices in bang boxes that get copied/renamed; 
             these have to be the same between lhs and rhs of a rule 
             when bboxes are matched/instantiated. subset of those in crnm. *)
          xmaps : VInjEndo.T B.NTab.T,
          xvs : V.NSet.T (* all exterior vertices of !-expanded pat *)
         };

val empty = 
    BBoxMatch
      {bbmap = BMap.empty,
       copies = BMap.empty,
       kill = B.NSet.empty,
       drop = B.NSet.empty,
       crnm = B.NTab.empty,
       xmaps = B.NTab.empty,
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
          (B.NSet.fold 
             (fn n => fn bbm => BMap.add_to_dom n n bbm)
             ns BMap.empty)
          |> set_xvs xvs;

fun init_with_exterior xvs = 
    empty |> set_xvs xvs;

fun init_from_pat g = 
    empty |> set_xvs (G.get_boundary g);



(* 
val copy = update_drop o B.NSet.add;
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
      Pretty.block [ Pretty.str "1. copies:", BMap.pretty (#copies rep) ],
      Pretty.block [ Pretty.str "2. kill:", B.NSet.pretty (#kill rep) ],
      Pretty.block [ Pretty.str "3. drop:", B.NSet.pretty (#drop rep) ],
      Pretty.block [ Pretty.str "4. bbmap:", BMap.pretty (#bbmap rep) ],
      Pretty.block [ Pretty.str "[xvs:", 
                     V.NSet.pretty (#xvs rep), 
                     Pretty.str "]" ],
      Pretty.block [ Pretty.str "[xmap:", 
                     B.NTab.pretty VInjEndo.pretty (#xmaps rep), 
                     Pretty.str "]" ],
      Pretty.block [ Pretty.str "[crnm:", 
                     B.NTab.pretty pretty_1crnm (#crnm rep), 
                     Pretty.str "]" ],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;


(* implicit args: ns m *)
val add_bbmap = update_bbmap oo BMap.add_to_dom;
val add_kills = update_kill o fold B.NSet.add;
val add_drop = update_drop o B.NSet.add;
fun add_copy (bbn1,(vtab,etab),bbn2) m =
    let val xvs = get_xvs m 
        val xmaps = get_xmaps m
        val xmap = (case B.NTab.lookup xmaps bbn2 of NONE => VInjEndo.empty
                      | SOME xmap => xmap); 
        val (xvs2,xmap2) = 
              V.NTab.fold 
                (fn (n1,n2) => fn (x as (xvs,xmap)) => 
                 if V.NSet.contains xvs n1
                 then (V.NSet.add n2 xvs, VInjEndo.add n1 n2 xmap) else x)
                vtab (xvs,xmap);
    in
      m |> update_copies (BMap.add_to_dom bbn2 bbn1)
        |> update_crnm (B.NTab.update (bbn2,(vtab,etab)))
        |> set_xvs xvs2 
        |> update_xmaps (B.NTab.update (bbn2,xmap2))
    end
    handle (ex as V.NSet.duplicate_exp (n,s)) => 
      (writeln "BUG: V.NSet.duplicate_exp"; V.print_name n; V.NSet.print s; print m; V.NTab.print V.pretty_name vtab; 
       E.NTab.print E.pretty_name etab;
       B.print_name bbn1; B.print_name bbn2;        
       raise ex);


  (* given a bbox matching against a graph, apply it: copy, kill and 
     drop bboxes. When copying, use the specified names. *)
  fun inst_pat_bbox_match bbmatch g = 
      g |> B.NTab.fold 
            (fn (bn2,bn1) => fn g => 
              let 
                val (vtab,etab) = 
                  B.NTab.get (get_crnm bbmatch) bn2
                val vrn = V.Rnm.mk_from_nmap vtab
                val ern = E.Rnm.mk_from_nmap etab
                val brn = B.Rnm.do_compose_rename1 bn1 bn2 B.Rnm.empty
                val (_,(_,g)) = G.copy1_box bn1 ((vrn,ern,brn),g)
              in g end)
            (BMap.get_domtab (get_copies bbmatch))
        |> B.NSet.fold G.kill_box (get_kill bbmatch)
        |> B.NSet.fold G.drop_box (get_drop bbmatch);

  (* makes sure to copy external/boundary vertex names exactly, 
     but other vertices are named arbitrarily, but fresh.
     Note: can copy exterior half-edge vertex names exactly because same 
     names are used on lhs and rhs. But internal part of bangbox may have 
     more vertices than lhs, so can't re-use same name-space.  
  *)
  fun inst_replacement_bbox_match bbmatch ((avoid_vs,avoid_es), g) = 
      let 
        val bb_xmaps = get_xmaps bbmatch;
        val bb_xvs = get_xvs bbmatch; (* all exterior vs *)
      in
        ((avoid_vs,avoid_es),g)
          |> B.NTab.fold (* copy bang-boxes, preserve interface names *)
              (fn (bn2,bn1) => fn ((avoid_vs,avoid_es),g) =>
               let 
                 val bb_nmap = VInjEndo.get_domtab 
                                (B.NTab.get bb_xmaps bn2);
                 val vrn = V.Rnm.mk V.NSet.empty avoid_vs bb_nmap;
                 val ern = E.Rnm.mk_from_avoids avoid_es;
                 val brn = B.Rnm.do_compose_rename1 bn1 bn2 B.Rnm.empty
                 val (_,((vrn,ern,brn),g)) = 
                     G.copy1_box bn1 ((vrn,ern,brn),g)
               in ((V.Rnm.get_avoids vrn, E.Rnm.get_avoids ern), g) end)
              (BMap.get_domtab (get_copies bbmatch))
          ||> (B.NSet.fold G.kill_box (get_kill bbmatch))
          ||> (B.NSet.fold G.drop_box (get_drop bbmatch))
      end;

  

structure Sharing = 
struct
  type T = T
  structure G = G.SharingOGraph
  structure BBMap = BMap.Sharing
end;


end;

