(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
(*  Data for !-Box matching: *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
signature BBOX_MATCH 
= sig

type T

val empty : T
val mk_id : BBox.NSet.T -> T

end;


(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-==-=-=-=-=-=-=-=-=-=- *) 
structure BBoxMatch 
: BBOX_MATCH
= struct

datatype T = BBoxMatch of
         {(* bang box isomorphism tgt -> pat *)
          iso : BBox.Rnm.T,
          copies : BBox.NSet.T BBox.NTab.T,
          kill : BBox.NSet.T,
          drop : BBox.NSet.T
         };
val empty = 
    BBoxMatch
      {iso = BBox.Rnm.empty,
       copies = BBox.NTab.empty,
       kill = BBox.NSet.empty,
       drop = BBox.NSet.empty
      };

fun get_iso (BBoxMatch rep) = #iso rep;
fun get_copies (BBoxMatch rep) = #copies rep;
fun get_kill (BBoxMatch rep) = #kill rep;
fun get_drop (BBoxMatch rep) = #drop rep;

fun update_iso f (BBoxMatch rep) = 
     BBoxMatch
      {iso = f( #iso rep),
       copies = #copies rep,
       kill = #kill rep,
       drop = #drop rep
      };
fun update_copies f (BBoxMatch rep) = 
     BBoxMatch
      {iso = #iso rep,
       copies = f (#copies rep),
       kill = #kill rep,
       drop = #drop rep
      };
fun update_kill f (BBoxMatch rep) = 
     BBoxMatch
      {iso = #iso rep,
       copies = #copies rep,
       kill = f (#kill rep),
       drop = #drop rep
      };
fun update_drop f (BBoxMatch rep) = 
     BBoxMatch
      {iso = #iso rep,
       copies = #copies rep,
       kill = #kill rep,
       drop = f (#drop rep)
      };

val set_iso = update_iso o K;
val set_copies = update_copies o K;
val set_kill = update_kill o K;
val set_drop = update_drop o K;

fun mk_id ns = 
    empty |> set_iso 
          (BBox.NSet.fold 
             (fn n => fn rn => the (BBox.Rnm.try_rename1 n n rn))
             ns BBox.Rnm.empty);
end;

