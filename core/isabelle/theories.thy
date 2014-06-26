theory theories
imports rewriting  
uses

(* TO DO: should really skip - but causes failures later... *)


(* string vertex/edge graphs *)
 "../../core/theories/string_ve/data.ML"
 "../../core/theories/string_ve/io.ML"
 "../../core/theories/string_ve/theory.ML"

(* red-green specific vertices, graphs and matching *)
(* graph-derived expressions for R-G graphs *)
 "../../core/theories/red_green/data.ML"
 "../../core/theories/red_green/io.ML"
 "../../core/theories/red_green/theory.ML"


(* ghz-w specific vertices, graphs, and matching *)
 "../../core/theories/ghz_w/data.ML"
 "../../core/theories/ghz_w/io.ML"
 "../../core/theories/ghz_w/theory.ML"


(* Graphs having vertices with strings as data, substring as matching *)
 "../../core/theories/substrings/data.ML"
 "../../core/theories/substrings/io.ML"
 "../../core/theories/substrings/theory.ML"

(* Graphs having strings as types, linrat as data and both substrings and linrat
 * as matching *)
 "../../core/theories/substr_linrat/data.ML"
 "../../core/theories/substr_linrat/io.ML"
 "../../core/theories/substr_linrat/theory.ML"

(* rgb specific vertices, graphs, and matching *)
 "../../core/theories/red_green_blue/data.ML"
 "../../core/theories/red_green_blue/io.ML"
 "../../core/theories/red_green_blue/theory.ML"

(* petri specific vertices, graphs, and matching *)
 "../../core/theories/petri/data.ML"
 "../../core/theories/petri/io.ML"
 "../../core/theories/petri/theory.ML"

(* Tactics as Graphs in Isabelle *)
 "../../core/theories/isaplanner_rtechn/data.ML"
 "../../core/theories/isaplanner_rtechn/io.ML"
 "../../core/theories/isaplanner_rtechn/theory.ML"


begin

end;

