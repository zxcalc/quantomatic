cd "isap_src";
(* Generic Tools for namers, fresh names tables, and collections *)
(* for creating fresh names, has name suc and pred operation, 
   also nameset with ability to make fresh names. *)
use "fnamer.ML"; 
use "namers.ML"; (* instances of fnamer *)
use "fnametab.ML"; (* name tables which provide fresh names *)

(* generic Name structure; provies nametables, namesets and collections *)
use "basic_names.ML";
use "renaming.ML"; (* genereic renaming for basic names *)
use "names.ML"; (* names + renaming for them *)

(* collections of things, without suc operation.  *)
use "collection.ML"; 

(* Relation (multi-edge) graphs *)
use "pregraph.ML";
use "rgraph.ML";

(* generic search *)
use "gsearch.ML";
use "msearch.ML";
use "lsearch.ML";
cd "..";
