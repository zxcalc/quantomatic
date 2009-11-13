(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Data for !-Box matching: *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(* information about bang-box matching in a graph *)
signature BBOX_MATCH_SHARING = 
sig
  structure BBox : NAMES_SHARING
  structure BBMap : NAME_MAP_SHARING
  type T

  sharing BBMap.Dom = BBox;
  sharing BBMap.Cod = BBox;

end;

(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BBOX_MATCH 
= sig

type T (* bang box match *)

structure BBox : SSTR_NAMES
structure BBMap : NAME_MAP


structure Sharing : BBOX_MATCH_SHARING
sharing type T = T
sharing Sharing.BBMap = BBMap.Sharing
sharing Sharing.BBox = BBox.Sharing
(* below 2 not needed; just for completeness *)
sharing BBMap.Dom.Sharing = BBox.Sharing;
sharing BBMap.Cod.Sharing = BBox.Sharing;

val get_bbmap : T -> BBMap.T;
val update_bbmap : (BBMap.T -> BBMap.T) -> T -> T;

val empty : T;
val mk_id : BBox.NSet.T -> T;
val add_kills : BBox.name list -> T -> T;
val add_drop : BBox.name -> T -> T;
val add_bbmap : BBox.name -> BBox.name -> T -> T;
val add_copy : BBox.name -> BBox.name -> T -> T;

val pretty : T -> Pretty.T
val print : T -> unit


end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor BBoxMatchFun(BBox : SSTR_NAMES)
: BBOX_MATCH
= struct

structure BBox = BBox;
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
          bbmap : BBMap.T (* 4. mapping of bboxes in extended pat to tgt *)
         };

val empty = 
    BBoxMatch
      {bbmap = BBMap.empty,
       copies = BBMap.empty,
       kill = BBox.NSet.empty,
       drop = BBox.NSet.empty
      };

fun get_bbmap (BBoxMatch rep) = #bbmap rep;
fun get_copies (BBoxMatch rep) = #copies rep;
fun get_kill (BBoxMatch rep) = #kill rep;
fun get_drop (BBoxMatch rep) = #drop rep;

fun update_bbmap f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = f( #bbmap rep),
       copies = #copies rep,
       kill = #kill rep,
       drop = #drop rep
      };
fun update_copies f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = f (#copies rep),
       kill = #kill rep,
       drop = #drop rep
      };
fun update_kill f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = #copies rep,
       kill = f (#kill rep),
       drop = #drop rep
      };
fun update_drop f (BBoxMatch rep) = 
     BBoxMatch
      {bbmap = #bbmap rep,
       copies = #copies rep,
       kill = #kill rep,
       drop = f (#drop rep)
      };

val set_bbmap = update_bbmap o K;
val set_copies = update_copies o K;
val set_kill = update_kill o K;
val set_drop = update_drop o K;

fun mk_id ns = 
    empty |> set_bbmap
          (BBox.NSet.fold 
             (fn n => fn bbm => BBMap.add_to_dom n n bbm)
             ns BBMap.empty);

(* implicit args: ns m *)
val add_bbmap = update_bbmap oo BBMap.add_to_dom;
val add_copy = update_copies oo BBMap.add_to_dom;
val add_kills = update_kill o fold BBox.NSet.add;
val add_drop = update_drop o BBox.NSet.add;

(* 
val copy = update_drop o BBox.NSet.add;
*)

(* *)
fun pretty (BBoxMatch rep) = 
    Pretty.chunks
    [ Pretty.str "BBoxMap: {",
      Pretty.block [ Pretty.str "1. copies:", BBMap.pretty (#copies rep) ],
      Pretty.block [ Pretty.str "2. kill:", BBox.NSet.pretty (#kill rep) ],
      Pretty.block [ Pretty.str "3. drop:", BBox.NSet.pretty (#drop rep) ],
      Pretty.block [ Pretty.str "4. bbmap:", BBMap.pretty (#bbmap rep) ],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;


structure Sharing = 
struct
  type T = T
  structure BBox = BBox.Sharing
  structure BBMap = BBMap.Sharing
end;


end;

