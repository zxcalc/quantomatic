(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Data for !-Box matching: *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 

(* information about bang-box matching in a graph *)
signature BBOX_MATCH 
= sig

type T (* bang box match *)

structure BBox : SSTR_NAMES
structure BBMap : NAME_MAP
sharing BBMap.Dom.Sharing = BBox.Sharing;
sharing BBMap.Cod.Sharing = BBox.Sharing;

val get_bbmap : T -> BBMap.T;
val update_bbmap : (BBMap.T -> BBMap.T) -> T -> T;

val empty : T;
val mk_id : BBox.NSet.T -> T;
val add_kills : BBox.name list -> T -> T;

val pretty : T -> Pretty.T
val print : T -> unit

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
functor BBoxMatchFun(
  structure BBox : SSTR_NAMES 
  and BBMap : NAME_MAP 
  sharing BBMap.Dom.Sharing = BBox.Sharing
  sharing BBMap.Cod.Sharing = BBox.Sharing
)
: BBOX_MATCH
= struct

structure BBox = BBox;
structure BBMap = BBMap;

datatype T = BBoxMatch of
         {
          bbmap : BBMap.T, (* mapping of bboxes in pat to tgt *)
          copies : BBMap.T, (* pattern copies *)
          kill : BBox.NSet.T,
          drop : BBox.NSet.T
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
val add_kills = update_kill o fold BBox.NSet.add;

(* *)
fun pretty (BBoxMatch rep) = 
    Pretty.chunks
    [ Pretty.str "BBoxMap: {",
      Pretty.block [ Pretty.str "bbmap:", BBMap.pretty (#bbmap rep) ],
      Pretty.block [ Pretty.str "copies:", BBMap.pretty (#copies rep) ],
      Pretty.block [ Pretty.str "kill:", BBox.NSet.pretty (#kill rep) ],
      Pretty.block [ Pretty.str "kill:", BBox.NSet.pretty (#drop rep) ],
      Pretty.str "}"
    ];

val print = Pretty.writeln o pretty;

end;

