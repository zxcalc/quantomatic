(*  Title:      Pure/Syntax/mixfix.ML
    Author:     Tobias Nipkow and Markus Wenzel, TU Muenchen

Mixfix declarations, infixes, binders.
*)

signature BASIC_MIXFIX =
sig
  datatype mixfix =
    NoSyn |
    Mixfix of string * int list * int |
    Delimfix of string |
    Infix of string * int |
    Infixl of string * int |
    Infixr of string * int |
    Binder of string * int * int |
    Structure
end;

signature MIXFIX =
sig
  include BASIC_MIXFIX
  val pretty_mixfix: mixfix -> Pretty.T
  val mixfix_args: mixfix -> int
  val mixfixT: mixfix -> typ
  val make_type: int -> typ
  val binder_name: string -> string
  val syn_ext_types: (string * typ * mixfix) list -> Syntax_Ext.syn_ext
  val syn_ext_consts: string list -> (string * typ * mixfix) list -> Syntax_Ext.syn_ext
end;

structure Mixfix: MIXFIX =
struct

(** mixfix declarations **)

datatype mixfix =
  NoSyn |
  Mixfix of string * int list * int |
  Delimfix of string |
  Infix of string * int |
  Infixl of string * int |
  Infixr of string * int |
  Binder of string * int * int |
  Structure;


(* pretty_mixfix *)

local

val quoted = Pretty.quote o Pretty.str;
val keyword = Pretty.keyword2;
val parens = Pretty.enclose "(" ")";
val brackets = Pretty.enclose "[" "]";
val int = Pretty.str o string_of_int;

in

fun pretty_mixfix NoSyn = Pretty.str ""
  | pretty_mixfix (Mixfix (s, ps, p)) =
      parens (Pretty.breaks [quoted s, brackets (Pretty.commas (map int ps)), int p])
  | pretty_mixfix (Delimfix s) = parens [quoted s]
  | pretty_mixfix (Infix (s, p)) = parens (Pretty.breaks [keyword "infix", quoted s, int p])
  | pretty_mixfix (Infixl (s, p)) = parens (Pretty.breaks [keyword "infixl", quoted s, int p])
  | pretty_mixfix (Infixr (s, p)) = parens (Pretty.breaks [keyword "infixl", quoted s, int p])
  | pretty_mixfix (Binder (s, p1, p2)) =
      parens (Pretty.breaks [keyword "binder", quoted s, brackets [int p1], int p2])
  | pretty_mixfix Structure = parens [keyword "structure"];

end;


(* syntax specifications *)

fun mixfix_args NoSyn = 0
  | mixfix_args (Mixfix (sy, _, _)) = Syntax_Ext.mfix_args sy
  | mixfix_args (Delimfix sy) = Syntax_Ext.mfix_args sy
  | mixfix_args (Infix (sy, _)) = 2 + Syntax_Ext.mfix_args sy
  | mixfix_args (Infixl (sy, _)) = 2 + Syntax_Ext.mfix_args sy
  | mixfix_args (Infixr (sy, _)) = 2 + Syntax_Ext.mfix_args sy
  | mixfix_args (Binder _) = 1
  | mixfix_args Structure = 0;

fun mixfixT (Binder _) = (dummyT --> dummyT) --> dummyT
  | mixfixT mx = replicate (mixfix_args mx) dummyT ---> dummyT;


(* syn_ext_types *)

val typeT = Type ("type", []);
fun make_type n = replicate n typeT ---> typeT;

fun syn_ext_types type_decls =
  let
    fun mk_infix sy ty t p1 p2 p3 = Syntax_Ext.Mfix ("(_ " ^ sy ^ "/ _)", ty, t, [p1, p2], p3);

    fun mfix_of (_, _, NoSyn) = NONE
      | mfix_of (t, ty, Mixfix (sy, ps, p)) = SOME (Syntax_Ext.Mfix (sy, ty, t, ps, p))
      | mfix_of (t, ty, Delimfix sy) = SOME (Syntax_Ext.Mfix (sy, ty, t, [], 1000))
      | mfix_of (t, ty, Infix (sy, p)) = SOME (mk_infix sy ty t (p + 1) (p + 1) p)
      | mfix_of (t, ty, Infixl (sy, p)) = SOME (mk_infix sy ty t p (p + 1) p)
      | mfix_of (t, ty, Infixr (sy, p)) = SOME (mk_infix sy ty t (p + 1) p p)
      | mfix_of (t, _, _) = error ("Bad mixfix declaration for " ^ quote t);

    fun check_args (_, ty, _) (SOME (mfix as Syntax_Ext.Mfix (sy, _, _, _, _))) =
          if length (Term.binder_types ty) = Syntax_Ext.mfix_args sy then ()
          else Syntax_Ext.err_in_mfix "Bad number of type constructor arguments" mfix
      | check_args _ NONE = ();

    val mfix = map mfix_of type_decls;
    val _ = map2 check_args type_decls mfix;
    val consts = map (fn (t, _, _) => (t, "")) type_decls;
  in Syntax_Ext.syn_ext (map_filter I mfix) consts ([], [], [], []) ([], []) end;


(* syn_ext_consts *)

val binder_stamp = stamp ();
val binder_name = suffix "_binder";

fun syn_ext_consts logical_types const_decls =
  let
    fun mk_infix sy ty c p1 p2 p3 =
      [Syntax_Ext.Mfix ("op " ^ sy, ty, c, [], 1000),
       Syntax_Ext.Mfix ("(_ " ^ sy ^ "/ _)", ty, c, [p1, p2], p3)];

    fun binder_typ _ (Type ("fun", [Type ("fun", [_, ty2]), ty3])) =
          [Type ("idts", []), ty2] ---> ty3
      | binder_typ c _ = error ("Bad type of binder: " ^ quote c);

    fun mfix_of (_, _, NoSyn) = []
      | mfix_of (c, ty, Mixfix (sy, ps, p)) = [Syntax_Ext.Mfix (sy, ty, c, ps, p)]
      | mfix_of (c, ty, Delimfix sy) = [Syntax_Ext.Mfix (sy, ty, c, [], 1000)]
      | mfix_of (c, ty, Infix (sy, p)) = mk_infix sy ty c (p + 1) (p + 1) p
      | mfix_of (c, ty, Infixl (sy, p)) = mk_infix sy ty c p (p + 1) p
      | mfix_of (c, ty, Infixr (sy, p)) = mk_infix sy ty c (p + 1) p p
      | mfix_of (c, ty, Binder (sy, p, q)) =
          [Syntax_Ext.Mfix ("(3" ^ sy ^ "_./ _)", binder_typ c ty, (binder_name c), [0, p], q)]
      | mfix_of (c, _, _) = error ("Bad mixfix declaration for " ^ quote c);

    fun binder (c, _, Binder _) = SOME (binder_name c, c)
      | binder _ = NONE;

    val mfix = maps mfix_of const_decls;
    val binders = map_filter binder const_decls;
    val binder_trs = binders
      |> map (Syntax_Ext.stamp_trfun binder_stamp o Syntax_Trans.mk_binder_tr);
    val binder_trs' = binders
      |> map (Syntax_Ext.stamp_trfun binder_stamp o
          apsnd Syntax_Trans.non_typed_tr' o Syntax_Trans.mk_binder_tr' o swap);

    val consts = binders @ map (fn (c, _, _) => (c, "")) const_decls;
  in
    Syntax_Ext.syn_ext' logical_types mfix consts ([], binder_trs, binder_trs', []) ([], [])
  end;

end;

structure Basic_Mixfix: BASIC_MIXFIX = Mixfix;
open Basic_Mixfix;

