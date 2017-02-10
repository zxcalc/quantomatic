theory theories
imports core
begin

(* string vertex/edge graphs *)
ML_file "theories/string_ve/data.ML"
ML_file "theories/string_ve/io.ML"
ML_file "theories/string_ve/theory.ML"
(*ML_file "theories/string_ve/test/test.ML";*)

(* red-green specific vertices, graphs and matching *)
(* graph-derived expressions for R-G graphs *)
ML_file "theories/red_green/data.ML"
ML_file "theories/red_green/io.ML"
ML_file "theories/red_green/theory.ML"
(*ML_file "theories/red_green/test/test.ML";*)
ML_file "theories/red_green/rg_mathematica.ML"

(* ghz-w specific vertices, graphs, and matching *)
ML_file "theories/ghz_w/data.ML"
ML_file "theories/ghz_w/io.ML"
ML_file "theories/ghz_w/theory.ML"
(*ML_file "theories/ghz_w/test/test.ML";*)

(* Graphs having vertices with strings as data, substring as matching *)
ML_file "theories/substrings/data.ML"
ML_file "theories/substrings/io.ML"
ML_file "theories/substrings/theory.ML"
(*ML_file "theories/substrings/test/test.ML";*)

(* Graphs having strings as types, linrat as data and both substrings and linrat
 * as matching *)
ML_file "theories/substr_linrat/data.ML"
ML_file "theories/substr_linrat/io.ML"
ML_file "theories/substr_linrat/theory.ML"
(*ML_file "theories/substr_linrat/test/test.ML";*)

(* rgb specific vertices, graphs, and matching *)
ML_file "theories/red_green_blue/data.ML"
ML_file "theories/red_green_blue/io.ML"
ML_file "theories/red_green_blue/theory.ML"
(*ML_file "theories/red_green_blue/test/test.ML";*)

(* petri specific vertices, graphs, and matching *)
ML_file "theories/petri/data.ML"
ML_file "theories/petri/io.ML"
ML_file "theories/petri/theory.ML"
(*ML_file "theories/petri/test/test.ML";*)

(* Tactics as Graphs in Isabelle *)
ML_file "theories/isaplanner_rtechn/data.ML"
ML_file "theories/isaplanner_rtechn/io.ML"
ML_file "theories/isaplanner_rtechn/theory.ML"
(*ML_file "theories/isaplanner_rtechn/test/test.ML";*)


(* Pair of dots with rational expressions *)
ML_file "theories/rational_pair/data.ML"
ML_file "theories/rational_pair/io.ML"
ML_file "theories/rational_pair/theory.ML"
(*ML_file "theories/rational_pair/test/test.ML";*)

end
