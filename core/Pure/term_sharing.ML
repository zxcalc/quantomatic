(*  Title:      Pure/term_sharing.ML
    Author:     Makarius

Local sharing of type/term sub-structure, with global interning of
formal entity names.
*)

signature TERM_SHARING =
sig
  val init: theory -> (typ -> typ) * (term -> term)
  val typs: theory -> typ list -> typ list
  val terms: theory -> term list -> term list
end;

structure Term_Sharing: TERM_SHARING =
struct

structure Syntax_Termtab = Table(type key = term val ord = Term_Ord.syntax_term_ord);

fun init thy =
  let
    val {classes = (_, algebra), types, ...} = Type.rep_tsig (Sign.tsig_of thy);

    val class = perhaps (try (#1 o Graph.get_entry (Sorts.classes_of algebra)));
    val tycon = perhaps (Option.map #1 o Name_Space.lookup_key types);
    val const = perhaps (try (#1 o Consts.the_const (Sign.consts_of thy)));

    val typs = Unsynchronized.ref (Typtab.empty: unit Typtab.table);
    val terms = Unsynchronized.ref (Syntax_Termtab.empty: unit Syntax_Termtab.table);

    fun typ T =
      (case Typtab.lookup_key (! typs) T of
        SOME (T', ()) => T'
      | NONE =>
          let
            val T' =
              (case T of
                Type (a, Ts) => Type (tycon a, map typ Ts)
              | TFree (a, S) => TFree (a, map class S)
              | TVar (a, S) => TVar (a, map class S));
            val _ = Unsynchronized.change typs (Typtab.update (T', ()));
          in T' end);

    fun term tm =
      (case Syntax_Termtab.lookup_key (! terms) tm of
        SOME (tm', ()) => tm'
      | NONE =>
          let
            val tm' =
              (case tm of
                Const (c, T) => Const (const c, typ T)
              | Free (x, T) => Free (x, typ T)
              | Var (xi, T) => Var (xi, typ T)
              | Bound i => Bound i
              | Abs (x, T, t) => Abs (x, typ T, term t)
              | t $ u => term t $ term u);
            val _ = Unsynchronized.change terms (Syntax_Termtab.update (tm', ()));
          in tm' end);

    fun check eq f x =
      let val x' = f x in
        if eq (x, x') then x'
        else raise Fail "Something is utterly wrong"
      end;

  in (check (op =) typ, check (op =) term) end;

val typs = map o #1 o init;
val terms = map o #2 o init;

end;

