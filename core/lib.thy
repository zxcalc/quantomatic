theory lib
imports Pure
begin

ML_file "lib/log.ML";
ML_file "lib/testing.ML";
ML_file "lib/raw_source.ML";
ML_file "lib/json.ML";
ML_file "lib/text_socket.ML";

(* Generic Tools for namers, fresh names tables, and collections *)
(* for creating fresh names, has name suc and pred operation, 
   also nameset with ability to make fresh names. *)

ML_file "lib/names/namer.ML";
ML_file "lib/names/namers.ML"; (* instances of namer, StrName, etc *)

ML_file "lib/names/basic_nameset.ML"; (* basic sets of names *)  
ML_file "lib/names/basic_nametab.ML"; (* name tables which provide fresh names *)
ML_file "lib/names/basic_renaming.ML"; (* renaming, based on tables and sets *)

(* generic Name structure provies nametables, namesets and collections *)
ML_file "lib/names/basic_name.ML";
ML_file "lib/names/compound_renaming.ML"; (* renaming within datatypes *)
ML_file "lib/names/renaming.ML"; (* renamings which can be renamed *)

(* as above, but with renaming *)
ML_file "lib/names/nameset.ML"; 
ML_file "lib/names/nametab.ML"; 

(* names + renaming for them, their tables, sets, and renamings *)
ML_file "lib/names/names.ML";

(* Binary Relations of finite name sets: good for dependencies *)
ML_file "lib/names/name_map.ML"; (* functions/mappings on names *)
ML_file "lib/names/name_inj.ML"; (* name iso-morphisms *)
ML_file "lib/names/name_injendo.ML"; (* name auto-morphisms (name iso where dom = cod) *)
ML_file "lib/names/name_binrel.ML"; (* bin relations on names *)

(* Defines SStrName, StrName, StrIntName and common maps. *)
ML_file "lib/names/names_common.ML"; 

(* testing *)
(*PolyML.Project.use_root "test/ROOT.ML";*)

ML_file "lib/maps/abstract_map.ML";
ML_file "lib/maps/name_table.ML";
ML_file "lib/maps/name_relation.ML";
ML_file "lib/maps/name_function.ML";
ML_file "lib/maps/name_injection.ML";
ML_file "lib/maps/name_substitution.ML";

end
