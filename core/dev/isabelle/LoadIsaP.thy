theory LoadIsaP
imports Main
uses   
   "../../../../isaplib/basics/log.ML" 
   "../../../../isaplib/project/project.ML" 
   "../../../../isaplib/project/testing.ML" 

   (* names *)
   "../../../../isaplib/names/namer.ML" 
   "../../../../isaplib/names/namers.ML"  (* instances of namer, StrName, etc *)
   "../../../../isaplib/names/basic_nameset.ML" (* basic sets of names *)  
   "../../../../isaplib/names/basic_nametab.ML" (* name tables which provide fresh names *)
   "../../../../isaplib/names/basic_renaming.ML" (* renaming, based on tables and sets *)

(* generic Name structure; provies nametables, namesets and collections *)
 "../../../../isaplib/names/basic_names.ML"
 "../../../../isaplib/names/compound_renaming.ML" (* renaming within datatypes *)
 "../../../../isaplib/names/renaming.ML" (* renamings which can be renamed *)
(* as above, but with renaming *)
 "../../../../isaplib/names/nameset.ML" 
 "../../../../isaplib/names/nametab.ML" 

(* names + renaming for them, their tables, sets, and renamings *)
 "../../../../isaplib/names/names.ML"


(* Binary Relations of finite name sets: good for dependencies *)
 "../../../../isaplib/names/name_map.ML" (* functions/mappings on names *)
 "../../../../isaplib/names/name_inj.ML" (* name iso-morphisms *)
 "../../../../isaplib/names/name_injendo.ML" (* name auto-morphisms (name iso where dom = cod) *)
 "../../../../isaplib/names/name_binrel.ML" (* bin relations on names *)

 "../../../../isaplib/names/umorph.ML" (* functions/mappings on names *)


(* Binary Relations of finite name sets: good for dependencies *)
 (* "../../../../isaplib/names/name_iso.ML" *) (* name iso-morphisms *)
 (* "../../../../isaplib/names/name_amorph.ML" *) (* name auto-morphisms (name iso where dom = cod) *)

 (* graphs *)
 "../../../../isaplib/graph/pregraph.ML"
 "../../../../isaplib/graph/rgraph.ML"

(* Other basic top level things *)
 "../../../../isaplib/basics/collection.ML"
 "../../../../isaplib/basics/polym_table.ML"
 "../../../../isaplib/basics/toplevel.ML"
begin

end;
