(* cheeky pretty print, ignoring most of ML's arguments *)
(* PolyML.install_pp
  (fn ppargs => 
   fn limitDepth => 
   fn printElement => 
   fn obj => 
      Pretty.writeln (VName.pretty_name obj));
*)

structure AlgFormat =
struct
exception not_supported_exp
datatype format = MATHEMATICA | MATLAB | LATEX | PLAIN
fun of_string "mathematica" = MATHEMATICA
  | of_string "matlab" = MATLAB
  | of_string "latex" = LATEX
  | of_string "plain" = PLAIN
  | of_string _ = raise not_supported_exp
end

(* Basic stuff for an expression which might have variables *)
signature EXPR =
sig 
  structure VName : NAMES where type name = string;
  (* NOTE: 
     VName.T = type of variables allowed in expressions 
     T VName.NTab.T = mapping of vars to expressions *)
  exception BadExpression of string;
  type T; (* expressions *)
  val ord : T * T -> General.order;
  val eq : T -> T -> bool;
  val get_vars : T -> VName.NSet.T;
  val subs : T VName.NTab.T -> T -> T;
  
  (* matching expressions *)
  type match (* substitutions for variables in a pattern expression *)
  val empty_match : match
  val match : T -> T -> match -> match option;
  val subst : match -> T -> T

  (* like f o g; this should just composes substitutions *)
  val compose_matches : match -> match -> match;

  val pretty_match : match -> Pretty.T;
  val print_match : match -> unit;

  (* internal stuff *)
  val subst_of_match : match -> T VName.NTab.T; 
  val subst_in_match_pair : match -> T * T -> T * T;


  (* gaussian elimination *)
  val eliminate : T -> T -> T VName.NTab.T -> T VName.NTab.T option;

  val pretty : T -> Pretty.T;
  val pretty_math : AlgFormat.format -> T -> Pretty.T;
  val print : T -> unit;

  (* rename variables in an expression *)
  structure CRnm : COMPOUND_RENAMING 
    where type obj = T and type renaming = VName.Rnm.T

end
