(*  Title:      Pure/Syntax/local_syntax.ML
    Author:     Makarius

Local syntax depending on theory syntax, with special support for
implicit structure references.
*)

signature LOCAL_SYNTAX =
sig
  type T
  val syn_of: T -> Syntax.syntax
  val idents_of: T -> {structs: string list, fixes: string list}
  val init: theory -> T
  val rebuild: theory -> T -> T
  datatype kind = Type | Const | Fixed
  val add_syntax: theory -> (kind * (string * typ * mixfix)) list -> T -> T
  val set_mode: Syntax.mode -> T -> T
  val restore_mode: T -> T -> T
  val update_modesyntax: theory -> bool -> Syntax.mode ->
    (kind * (string * typ * mixfix)) list -> T -> T
end;

structure Local_Syntax: LOCAL_SYNTAX =
struct

(* datatype T *)

type local_mixfix =
  (string * bool) *  (*name, fixed?*)
  ((bool * bool * Syntax.mode) * (string * typ * mixfix));  (*type?, add?, mode, declaration*)

datatype T = Syntax of
 {thy_syntax: Syntax.syntax,
  local_syntax: Syntax.syntax,
  mode: Syntax.mode,
  mixfixes: local_mixfix list,
  idents: string list * string list};

fun make_syntax (thy_syntax, local_syntax, mode, mixfixes, idents) =
  Syntax {thy_syntax = thy_syntax, local_syntax = local_syntax,
    mode = mode, mixfixes = mixfixes, idents = idents};

fun map_syntax f (Syntax {thy_syntax, local_syntax, mode, mixfixes, idents}) =
  make_syntax (f (thy_syntax, local_syntax, mode, mixfixes, idents));

fun is_consistent thy (Syntax {thy_syntax, ...}) =
  Syntax.eq_syntax (Sign.syn_of thy, thy_syntax);

fun syn_of (Syntax {local_syntax, ...}) = local_syntax;
fun idents_of (Syntax {idents = (structs, fixes), ...}) = {structs = structs, fixes = fixes};


(* build syntax *)

fun build_syntax thy mode mixfixes (idents as (structs, _)) =
  let
    val thy_syntax = Sign.syn_of thy;
    fun update_gram ((true, add, m), decls) = Syntax.update_type_gram add m decls
      | update_gram ((false, add, m), decls) =
          Syntax.update_const_gram add (Sign.is_logtype thy) m decls;

    val local_syntax = thy_syntax
      |> Syntax.update_trfuns
          ([], [Syntax_Ext.mk_trfun (Syntax_Trans.struct_tr structs)],
           [], [Syntax_Ext.mk_trfun (Syntax_Trans.struct_ast_tr' structs)])
      |> fold update_gram (AList.coalesce (op =) (rev (map snd mixfixes)));
  in make_syntax (thy_syntax, local_syntax, mode, mixfixes, idents) end;

fun init thy = build_syntax thy Syntax.mode_default [] ([], []);

fun rebuild thy (syntax as Syntax {mode, mixfixes, idents, ...}) =
  if is_consistent thy syntax then syntax
  else build_syntax thy mode mixfixes idents;


(* mixfix declarations *)

datatype kind = Type | Const | Fixed;

local

fun prep_mixfix _ _ (_, (_, _, Structure)) = NONE
  | prep_mixfix add mode (Type, decl as (x, _, _)) = SOME ((x, false), ((true, add, mode), decl))
  | prep_mixfix add mode (Const, decl as (x, _, _)) = SOME ((x, false), ((false, add, mode), decl))
  | prep_mixfix add mode (Fixed, (x, T, mx)) =
      SOME ((x, true), ((false, add, mode), (Lexicon.mark_fixed x, T, mx)));

fun prep_struct (Fixed, (c, _, Structure)) = SOME c
  | prep_struct (_, (c, _, Structure)) = error ("Bad mixfix declaration for " ^ quote c)
  | prep_struct _ = NONE;

in

fun update_syntax add thy raw_decls
    (syntax as (Syntax {mode, mixfixes, idents = (structs, _), ...})) =
  (case filter_out (fn (_, (_, _, mx)) => mx = NoSyn) raw_decls of
    [] => syntax
  | decls =>
      let
        val new_mixfixes = map_filter (prep_mixfix add mode) decls;
        val new_structs = map_filter prep_struct decls;
        val mixfixes' = rev new_mixfixes @ mixfixes;
        val structs' =
          if add then structs @ new_structs
          else subtract (op =) new_structs structs;
        val fixes' = fold (fn ((x, true), _) => cons x | _ => I) mixfixes' [];
      in build_syntax thy mode mixfixes' (structs', fixes') end);

val add_syntax = update_syntax true;

end;


(* syntax mode *)

fun set_mode mode = map_syntax (fn (thy_syntax, local_syntax, _, mixfixes, idents) =>
  (thy_syntax, local_syntax, mode, mixfixes, idents));

fun restore_mode (Syntax {mode, ...}) = set_mode mode;

fun update_modesyntax thy add mode args syntax =
  syntax |> set_mode mode |> update_syntax add thy args |> restore_mode syntax;

end;
