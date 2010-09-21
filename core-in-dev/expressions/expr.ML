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
  val match : T -> T -> T VName.NTab.T -> T VName.NTab.T option;
  val eliminate : T -> T -> T VName.NTab.T -> T VName.NTab.T option;
  val pretty : T -> Pretty.T;
  val pretty_math : AlgFormat.format -> T -> Pretty.T;
  val print : T -> unit;

  (* rename variables in an expression *)
  structure RnmExpr : COMPOUND_RENAMING 
    where type obj = T and type renaming = VName.Rnm.T

end
