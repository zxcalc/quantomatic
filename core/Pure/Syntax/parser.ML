(*  Title:      Pure/Syntax/parser.ML
    Author:     Carsten Clasohm, Sonia Mahjoub, and Markus Wenzel, TU Muenchen

General context-free parser for the inner syntax of terms, types, etc.
*)

signature PARSER =
sig
  type gram
  val empty_gram: gram
  val extend_gram: Syntax_Ext.xprod list -> gram -> gram
  val make_gram: Syntax_Ext.xprod list -> gram
  val pretty_gram: gram -> Pretty.T list
  datatype parsetree =
    Node of string * parsetree list |
    Tip of Lexicon.token
  exception PARSETREE of parsetree
  val pretty_parsetree: parsetree -> Pretty.T
  val parse: gram -> string -> Lexicon.token list -> parsetree list
  val guess_infix_lr: gram -> string -> (string * bool * bool * int) option
  val branching_level: int Config.T
end;

structure Parser: PARSER =
struct

(** datatype gram **)

(*production for the NTs are stored in a vector
  so we can identify NTs by their index*)
type nt_tag = int;

datatype symb =
  Terminal of Lexicon.token
| Nonterminal of nt_tag * int;  (*(tag, precedence)*)

type nt_gram =
  ((nt_tag list * Lexicon.token list) *
    (Lexicon.token option * (symb list * string * int) list) list);
  (*(([dependent_nts], [start_tokens]), [(start_token, [(rhs, name, prio)])])*)
  (*depent_nts is a list of all NTs whose lookahead depends on this NT's lookahead*)

datatype gram =
  Gram of
   {nt_count: int,
    prod_count: int,
    tags: nt_tag Symtab.table,
    chains: (nt_tag * nt_tag list) list,  (*[(to, [from])]*)
    lambdas: nt_tag list,
    prods: nt_gram Vector.vector};
    (*"tags" is used to map NT names (i.e. strings) to tags;
     chain productions are not stored as normal productions
     but instead as an entry in "chains";
     lambda productions are stored as normal productions
     and also as an entry in "lambdas"*)

val union_token = union Lexicon.matching_tokens;
val subtract_token = subtract Lexicon.matching_tokens;

(*productions for which no starting token is
  known yet are associated with this token*)
val unknown_start = Lexicon.eof;

(*get all NTs that are connected with a list of NTs*)
fun connected_with _ ([]: nt_tag list) relatives = relatives
  | connected_with chains (root :: roots) relatives =
      let val branches = subtract (op =) relatives (these (AList.lookup (op =) chains root));
      in connected_with chains (branches @ roots) (branches @ relatives) end;

(*convert productions to grammar;
  N.B. that the chains parameter has the form [(from, [to])];
  prod_count is of type "int option" and is only updated if it is <> NONE*)
fun add_prods _ chains lambdas prod_count [] = (chains, lambdas, prod_count)
  | add_prods prods chains lambdas prod_count ((lhs, new_prod as (rhs, name, pri)) :: ps) =
      let
        val chain_from =
          (case (pri, rhs) of
            (~1, [Nonterminal (id, ~1)]) => SOME id
          | _ => NONE);

        (*store chain if it does not already exist*)
        val (new_chain, chains') =
          (case chain_from of
            NONE => (NONE, chains)
          | SOME from =>
              let val old_tos = these (AList.lookup (op =) chains from) in
                if member (op =) old_tos lhs then (NONE, chains)
                else (SOME from, AList.update (op =) (from, insert (op =) lhs old_tos) chains)
              end);

        (*propagate new chain in lookahead and lambda lists;
          added_starts is used later to associate existing
          productions with new starting tokens*)
        val (added_starts, lambdas') =
          if is_none new_chain then ([], lambdas)
          else
            let (*lookahead of chain's source*)
              val ((from_nts, from_tks), _) = Array.sub (prods, the new_chain);

              (*copy from's lookahead to chain's destinations*)
              fun copy_lookahead [] added = added
                | copy_lookahead (to :: tos) added =
                    let
                      val ((to_nts, to_tks), ps) = Array.sub (prods, to);

                      val new_tks = subtract (op =) to_tks from_tks;  (*added lookahead tokens*)
                      val _ = Array.update (prods, to, ((to_nts, to_tks @ new_tks), ps));
                    in
                      copy_lookahead tos (if null new_tks then added else (to, new_tks) :: added)
                    end;

              val tos = connected_with chains' [lhs] [lhs];
            in
              (copy_lookahead tos [],
                union (op =) (if member (op =) lambdas lhs then tos else []) lambdas)
            end;

        (*test if new production can produce lambda
          (rhs must either be empty or only consist of lambda NTs)*)
        val (new_lambda, lambdas') =
          if forall
            (fn Nonterminal (id, _) => member (op =) lambdas' id
              | Terminal _ => false) rhs
          then (true, union (op =) (connected_with chains' [lhs] [lhs]) lambdas')
          else (false, lambdas');

        (*list optional terminal and all nonterminals on which the lookahead
          of a production depends*)
        fun lookahead_dependency _ [] nts = (NONE, nts)
          | lookahead_dependency _ (Terminal tk :: _) nts = (SOME tk, nts)
          | lookahead_dependency lambdas (Nonterminal (nt, _) :: symbs) nts =
              if member (op =) lambdas nt then
                lookahead_dependency lambdas symbs (nt :: nts)
              else (NONE, nt :: nts);

        (*get all known starting tokens for a nonterminal*)
        fun starts_for_nt nt = snd (fst (Array.sub (prods, nt)));

        (*update prods, lookaheads, and lambdas according to new lambda NTs*)
        val (added_starts', lambdas') =
          let
            (*propagate added lambda NT*)
            fun propagate_lambda [] added_starts lambdas = (added_starts, lambdas)
              | propagate_lambda (l :: ls) added_starts lambdas =
                  let
                    (*get lookahead for lambda NT*)
                    val ((dependent, l_starts), _) = Array.sub (prods, l);

                    (*check productions whose lookahead may depend on lambda NT*)
                    fun examine_prods [] add_lambda nt_dependencies added_tks nt_prods =
                          (add_lambda, nt_dependencies, added_tks, nt_prods)
                      | examine_prods ((p as (rhs, _, _)) :: ps) add_lambda
                            nt_dependencies added_tks nt_prods =
                          let val (tk, nts) = lookahead_dependency lambdas rhs [] in
                            if member (op =) nts l then       (*update production's lookahead*)
                              let
                                val new_lambda = is_none tk andalso subset (op =) (nts, lambdas);

                                val new_tks =
                                  (if is_some tk then [the tk] else [])
                                  |> fold (union_token o starts_for_nt) nts
                                  |> subtract (op =) l_starts;

                                val added_tks' = union_token added_tks new_tks;

                                val nt_dependencies' = union (op =) nts nt_dependencies;

                                (*associate production with new starting tokens*)
                                fun copy ([]: Lexicon.token option list) nt_prods = nt_prods
                                  | copy (tk :: tks) nt_prods =
                                      let
                                        val old_prods = these (AList.lookup (op =) nt_prods tk);
                                        val prods' = p :: old_prods;
                                      in
                                        nt_prods
                                        |> AList.update (op =) (tk, prods')
                                        |> copy tks
                                      end;

                                val nt_prods' =
                                  let val new_opt_tks = map SOME new_tks in
                                    copy
                                      ((if new_lambda then [NONE] else []) @ new_opt_tks) nt_prods
                                  end;
                              in
                                examine_prods ps (add_lambda orelse new_lambda)
                                  nt_dependencies' added_tks' nt_prods'
                              end
                            else (*skip production*)
                              examine_prods ps add_lambda nt_dependencies added_tks nt_prods
                          end;

                    (*check each NT whose lookahead depends on new lambda NT*)
                    fun process_nts [] added_lambdas added_starts =
                          (added_lambdas, added_starts)
                      | process_nts (nt :: nts) added_lambdas added_starts =
                          let
                            val (lookahead as (old_nts, old_tks), nt_prods) = Array.sub (prods, nt);

                            (*existing productions whose lookahead may depend on l*)
                            val tk_prods =
                              these
                                (AList.lookup (op =) nt_prods
                                  (SOME (hd l_starts handle List.Empty => unknown_start)));

                            (*add_lambda is true if an existing production of the nt
                              produces lambda due to the new lambda NT l*)
                            val (add_lambda, nt_dependencies, added_tks, nt_prods') =
                              examine_prods tk_prods false [] [] nt_prods;

                            val added_nts = subtract (op =) old_nts nt_dependencies;

                            val added_lambdas' =
                              if add_lambda then nt :: added_lambdas
                              else added_lambdas;
                            val _ =
                              Array.update
                                (prods, nt, ((added_nts @ old_nts, old_tks @ added_tks), nt_prods'));
                              (*N.B. that because the tks component
                                is used to access existing
                                productions we have to add new
                                tokens at the _end_ of the list*)
                          in
                            if null added_tks then
                              process_nts nts added_lambdas' added_starts
                            else
                              process_nts nts added_lambdas' ((nt, added_tks) :: added_starts)
                          end;

                    val (added_lambdas, added_starts') = process_nts dependent [] added_starts;
                    val added_lambdas' = subtract (op =) lambdas added_lambdas;
                  in
                    propagate_lambda (ls @ added_lambdas') added_starts' (added_lambdas' @ lambdas)
                  end;
          in propagate_lambda (subtract (op =) lambdas lambdas') added_starts lambdas' end;

        (*insert production into grammar*)
        val (added_starts', prod_count') =
          if is_some chain_from
          then (added_starts', prod_count)  (*don't store chain production*)
          else
            let
              (*lookahead tokens of new production and on which
                NTs lookahead depends*)
              val (start_tk, start_nts) = lookahead_dependency lambdas' rhs [];

              val start_tks =
                (if is_some start_tk then [the start_tk] else [])
                |> fold (union_token o starts_for_nt) start_nts;

              val opt_starts =
               (if new_lambda then [NONE]
                else if null start_tks then [SOME unknown_start]
                else []) @ map SOME start_tks;

              (*add lhs NT to list of dependent NTs in lookahead*)
              fun add_nts [] = ()
                | add_nts (nt :: nts) =
                    let val ((old_nts, old_tks), ps) = Array.sub (prods, nt) in
                      if member (op =) old_nts lhs then ()
                      else Array.update (prods, nt, ((lhs :: old_nts, old_tks), ps))
                    end;

              (*add new start tokens to chained NTs' lookahead list;
                also store new production for lhs NT*)
              fun add_tks [] added prod_count = (added, prod_count)
                | add_tks (nt :: nts) added prod_count =
                    let
                      val ((old_nts, old_tks), nt_prods) = Array.sub (prods, nt);

                      val new_tks = subtract_token old_tks start_tks;

                      (*store new production*)
                      fun store [] prods is_new =
                            (prods,
                              if is_some prod_count andalso is_new then
                                Option.map (fn x => x + 1) prod_count
                              else prod_count, is_new)
                        | store (tk :: tks) prods is_new =
                            let
                              val tk_prods = these (AList.lookup (op =) prods tk);

                              (*if prod_count = NONE then we can assume that
                                grammar does not contain new production already*)
                              val (tk_prods', is_new') =
                                if is_some prod_count then
                                  if member (op =) tk_prods new_prod then (tk_prods, false)
                                  else (new_prod :: tk_prods, true)
                                else (new_prod :: tk_prods, true);

                              val prods' =
                                if is_new' then
                                  AList.update (op =) (tk: Lexicon.token option, tk_prods') prods
                                else prods;
                            in store tks prods' (is_new orelse is_new') end;

                      val (nt_prods', prod_count', changed) =
                        if nt = lhs
                        then store opt_starts nt_prods false
                        else (nt_prods, prod_count, false);
                      val _ =
                        if not changed andalso null new_tks then ()
                        else Array.update (prods, nt, ((old_nts, old_tks @ new_tks), nt_prods'));
                    in
                      add_tks nts
                        (if null new_tks then added else (nt, new_tks) :: added) prod_count'
                    end;
              val _ = add_nts start_nts;
            in
              add_tks (connected_with chains' [lhs] [lhs]) [] prod_count
            end;

        (*associate productions with new lookaheads*)
        val _ =
          let
            (*propagate added start tokens*)
            fun add_starts [] = ()
              | add_starts ((changed_nt, new_tks) :: starts) =
                  let
                    (*token under which old productions which
                      depend on changed_nt could be stored*)
                    val key =
                      (case find_first (not o member (op =) new_tks) (starts_for_nt changed_nt) of
                        NONE => SOME unknown_start
                      | t => t);

                    (*copy productions whose lookahead depends on changed_nt;
                      if key = SOME unknown_start then tk_prods is used to hold
                      the productions not copied*)
                    fun update_prods [] result = result
                      | update_prods ((p as (rhs, _: string, _: nt_tag)) :: ps)
                            (tk_prods, nt_prods) =
                          let
                            (*lookahead dependency for production*)
                            val (tk, depends) = lookahead_dependency lambdas' rhs [];

                            (*test if this production has to be copied*)
                            val update = member (op =) depends changed_nt;

                            (*test if production could already be associated with
                              a member of new_tks*)
                            val lambda =
                              length depends > 1 orelse
                              not (null depends) andalso is_some tk
                              andalso member (op =) new_tks (the tk);

                            (*associate production with new starting tokens*)
                            fun copy ([]: Lexicon.token list) nt_prods = nt_prods
                              | copy (tk :: tks) nt_prods =
                                 let
                                   val tk_prods = these (AList.lookup (op =) nt_prods (SOME tk));

                                   val tk_prods' =
                                     if not lambda then p :: tk_prods
                                     else insert (op =) p tk_prods;
                                     (*if production depends on lambda NT we
                                       have to look for duplicates*)
                                 in
                                   nt_prods
                                   |> AList.update (op =) (SOME tk, tk_prods')
                                   |> copy tks
                                 end;
                            val result =
                              if update then (tk_prods, copy new_tks nt_prods)
                              else if key = SOME unknown_start then (p :: tk_prods, nt_prods)
                              else (tk_prods, nt_prods);
                          in update_prods ps result end;

                    (*copy existing productions for new starting tokens*)
                    fun process_nts [] added = added
                      | process_nts (nt :: nts) added =
                          let
                            val (lookahead as (old_nts, old_tks), nt_prods) = Array.sub (prods, nt);

                            val tk_prods = these (AList.lookup (op =) nt_prods key);

                            (*associate productions with new lookahead tokens*)
                            val (tk_prods', nt_prods') = update_prods tk_prods ([], nt_prods);

                            val nt_prods'' =
                              if key = SOME unknown_start then
                                AList.update (op =) (key, tk_prods') nt_prods'
                              else nt_prods';

                            val added_tks = subtract_token old_tks new_tks;
                          in
                            if null added_tks then
                              (Array.update (prods, nt, (lookahead, nt_prods''));
                                process_nts nts added)
                            else
                              (Array.update (prods, nt, ((old_nts, added_tks @ old_tks), nt_prods''));
                                process_nts nts ((nt, added_tks) :: added))
                          end;

                    val ((dependent, _), _) = Array.sub (prods, changed_nt);
                  in add_starts (starts @ process_nts dependent []) end;
          in add_starts added_starts' end;
      in add_prods prods chains' lambdas' prod_count ps end;


(* pretty_gram *)

fun pretty_gram (Gram {tags, prods, chains, ...}) =
  let
    fun pretty_name name = [Pretty.str (name ^ " =")];

    val nt_name = the o Inttab.lookup (Inttab.make (map swap (Symtab.dest tags)));

    fun pretty_symb (Terminal (Lexicon.Token (Lexicon.Literal, s, _))) = Pretty.quote (Pretty.str s)
      | pretty_symb (Terminal tok) = Pretty.str (Lexicon.str_of_token tok)
      | pretty_symb (Nonterminal (tag, p)) =
          Pretty.str (nt_name tag ^ "[" ^ signed_string_of_int p ^ "]");

    fun pretty_const "" = []
      | pretty_const c = [Pretty.str ("=> " ^ quote c)];

    fun pretty_pri p = [Pretty.str ("(" ^ signed_string_of_int p ^ ")")];

    fun pretty_prod name (symbs, const, pri) =
      Pretty.block (Pretty.breaks (pretty_name name @
        map pretty_symb symbs @ pretty_const const @ pretty_pri pri));

    fun pretty_nt (name, tag) =
      let
        fun prod_of_chain from = ([Nonterminal (from, ~1)], "", ~1);

        val nt_prods =
          fold (union (op =) o snd) (snd (Vector.sub (prods, tag))) [] @
            map prod_of_chain (these (AList.lookup (op =) chains tag));
      in map (pretty_prod name) nt_prods end;

  in maps pretty_nt (sort_wrt fst (Symtab.dest tags)) end;



(** Operations on gramars **)

val empty_gram =
  Gram
   {nt_count = 0,
    prod_count = 0,
    tags = Symtab.empty, chains = [],
    lambdas = [],
    prods = Vector.fromList [(([], []), [])]};


(*Invert list of chain productions*)
fun inverse_chains [] result = result
  | inverse_chains ((root, branches: nt_tag list) :: cs) result =
      let
        fun add ([]: nt_tag list) result = result
          | add (id :: ids) result =
              let val old = these (AList.lookup (op =) result id);
              in add ids (AList.update (op =) (id, root :: old) result) end;
      in inverse_chains cs (add branches result) end;


(*Add productions to a grammar*)
fun extend_gram [] gram = gram
  | extend_gram xprods (Gram {nt_count, prod_count, tags, chains, lambdas, prods}) =
      let
        (*Get tag for existing nonterminal or create a new one*)
        fun get_tag nt_count tags nt =
          (case Symtab.lookup tags nt of
            SOME tag => (nt_count, tags, tag)
          | NONE => (nt_count + 1, Symtab.update_new (nt, nt_count) tags, nt_count));

        (*Convert symbols to the form used by the parser;
          delimiters and predefined terms are stored as terminals,
          nonterminals are converted to integer tags*)
        fun symb_of [] nt_count tags result = (nt_count, tags, rev result)
          | symb_of (Syntax_Ext.Delim s :: ss) nt_count tags result =
              symb_of ss nt_count tags
                (Terminal (Lexicon.Token (Lexicon.Literal, s, Position.no_range)) :: result)
          | symb_of (Syntax_Ext.Argument (s, p) :: ss) nt_count tags result =
              let
                val (nt_count', tags', new_symb) =
                  (case Lexicon.predef_term s of
                    NONE =>
                      let val (nt_count', tags', s_tag) = get_tag nt_count tags s;
                      in (nt_count', tags', Nonterminal (s_tag, p)) end
                  | SOME tk => (nt_count, tags, Terminal tk));
              in symb_of ss nt_count' tags' (new_symb :: result) end
          | symb_of (_ :: ss) nt_count tags result = symb_of ss nt_count tags result;

        (*Convert list of productions by invoking symb_of for each of them*)
        fun prod_of [] nt_count prod_count tags result =
              (nt_count, prod_count, tags, result)
          | prod_of (Syntax_Ext.XProd (lhs, xsymbs, const, pri) :: ps)
                nt_count prod_count tags result =
              let
                val (nt_count', tags', lhs_tag) = get_tag nt_count tags lhs;
                val (nt_count'', tags'', prods) = symb_of xsymbs nt_count' tags' [];
              in
                prod_of ps nt_count'' (prod_count + 1) tags''
                  ((lhs_tag, (prods, const, pri)) :: result)
              end;

        val (nt_count', prod_count', tags', xprods') =
          prod_of xprods nt_count prod_count tags [];

        (*Copy array containing productions of old grammar;
          this has to be done to preserve the old grammar while being able
          to change the array's content*)
        val prods' =
          let
            fun get_prod i =
              if i < nt_count then Vector.sub (prods, i)
              else (([], []), []);
          in Array.tabulate (nt_count', get_prod) end;

        val fromto_chains = inverse_chains chains [];

        (*Add new productions to old ones*)
        val (fromto_chains', lambdas', _) =
          add_prods prods' fromto_chains lambdas NONE xprods';

        val chains' = inverse_chains fromto_chains' [];
      in
        Gram
         {nt_count = nt_count',
          prod_count = prod_count',
          tags = tags',
          chains = chains',
          lambdas = lambdas',
          prods = Array.vector prods'}
      end;

fun make_gram xprods = extend_gram xprods empty_gram;



(** parser **)

(* parsetree *)

datatype parsetree =
  Node of string * parsetree list |
  Tip of Lexicon.token;

exception PARSETREE of parsetree;

fun pretty_parsetree parsetree =
  let
    fun pretty (Node (c, pts)) =
          [Pretty.enclose "(" ")" (Pretty.breaks (Pretty.quote (Pretty.str c) :: maps pretty pts))]
      | pretty (Tip tok) =
          if Lexicon.valued_token tok then [Pretty.str (Lexicon.str_of_token tok)] else [];
  in (case pretty parsetree of [prt] => prt | _ => raise PARSETREE parsetree) end;


(* parser state *)

type state =
  nt_tag * int *    (*identification and production precedence*)
  parsetree list *  (*already parsed nonterminals on rhs*)
  symb list *       (*rest of rhs*)
  string *          (*name of production*)
  int;              (*index for previous state list*)


(*Get all rhss with precedence >= min_prec*)
fun get_RHS min_prec = filter (fn (_, _, prec: int) => prec >= min_prec);

(*Get all rhss with precedence >= min_prec and < max_prec*)
fun get_RHS' min_prec max_prec =
  filter (fn (_, _, prec: int) => prec >= min_prec andalso prec < max_prec);

(*Make states using a list of rhss*)
fun mk_states i min_prec lhs_ID rhss =
  let fun mk_state (rhs, id, prod_prec) = (lhs_ID, prod_prec, [], rhs, id, i);
  in map mk_state rhss end;

(*Add parse tree to list and eliminate duplicates
  saving the maximum precedence*)
fun conc (t: parsetree list, prec: int) [] = (NONE, [(t, prec)])
  | conc (t, prec) ((t', prec') :: ts) =
      if t = t' then
        (SOME prec',
          if prec' >= prec then (t', prec') :: ts
          else (t, prec) :: ts)
      else
        let val (n, ts') = conc (t, prec) ts
        in (n, (t', prec') :: ts') end;

(*Update entry in used*)
fun update_trees (A, t) used =
  let
    val (i, ts) = the (Inttab.lookup used A);
    val (n, ts') = conc t ts;
  in (n, Inttab.update (A, (i, ts')) used) end;

(*Replace entry in used*)
fun update_prec (A, prec) =
  Inttab.map_entry A (fn (_, ts) => (prec, ts));

fun getS A max_prec NONE Si =
      filter
        (fn (_, _, _, Nonterminal (B, prec) :: _, _, _) => A = B andalso prec <= max_prec
          | _ => false) Si
  | getS A max_prec (SOME min_prec) Si =
      filter
        (fn (_, _, _, Nonterminal (B, prec) :: _, _, _) =>
            A = B andalso prec > min_prec andalso prec <= max_prec
          | _ => false) Si;

fun get_states Estate i ii A max_prec =
  filter
    (fn (_, _, _, Nonterminal (B, prec) :: _, _, _) => A = B andalso prec <= max_prec
      | _ => false)
    (Array.sub (Estate, ii));


fun movedot_term c (A, j, ts, Terminal a :: sa, id, i) =
  if Lexicon.valued_token c orelse id <> ""
  then (A, j, Tip c :: ts, sa, id, i)
  else (A, j, ts, sa, id, i);

fun movedot_nonterm tt (A, j, ts, Nonterminal _ :: sa, id, i) =
  (A, j, tt @ ts, sa, id, i);

fun movedot_lambda [] _ = []
  | movedot_lambda ((t, ki) :: ts) (state as (B, j, tss, Nonterminal (A, k) :: sa, id, i)) =
      if k <= ki then (B, j, t @ tss, sa, id, i) :: movedot_lambda ts state
      else movedot_lambda ts state;


(*trigger value for warnings*)
val branching_level =
  Config.int (Config.declare ("syntax_branching_level", @{here}) (fn _ => Config.Int 600));

(*get all productions of a NT and NTs chained to it which can
  be started by specified token*)
fun prods_for prods chains include_none tk nts =
  let
    fun token_assoc (list, key) =
      let
        fun assoc [] result = result
          | assoc ((keyi, pi) :: pairs) result =
              if is_some keyi andalso Lexicon.matching_tokens (the keyi, key)
                 orelse include_none andalso is_none keyi then
                assoc pairs (pi @ result)
              else assoc pairs result;
      in assoc list [] end;

    fun get_prods [] result = result
      | get_prods (nt :: nts) result =
          let val nt_prods = snd (Vector.sub (prods, nt));
          in get_prods nts (token_assoc (nt_prods, tk) @ result) end;
  in get_prods (connected_with chains nts nts) [] end;


fun PROCESSS prods chains Estate i c states =
  let
    fun all_prods_for nt = prods_for prods chains true c [nt];

    fun processS used [] (Si, Sii) = (Si, Sii)
      | processS used (S :: States) (Si, Sii) =
          (case S of
            (_, _, _, Nonterminal (nt, min_prec) :: _, _, _) =>
              let (*predictor operation*)
                val (used', new_states) =
                  (case Inttab.lookup used nt of
                    SOME (used_prec, l) => (*nonterminal has been processed*)
                      if used_prec <= min_prec then
                        (*wanted precedence has been processed*)
                        (used, movedot_lambda l S)
                      else (*wanted precedence hasn't been parsed yet*)
                        let
                          val tk_prods = all_prods_for nt;
                          val States' =
                            mk_states i min_prec nt (get_RHS' min_prec used_prec tk_prods);
                        in (update_prec (nt, min_prec) used, movedot_lambda l S @ States') end
                  | NONE => (*nonterminal is parsed for the first time*)
                      let
                        val tk_prods = all_prods_for nt;
                        val States' = mk_states i min_prec nt (get_RHS min_prec tk_prods);
                      in (Inttab.update (nt, (min_prec, [])) used, States') end);
              in
                processS used' (new_states @ States) (S :: Si, Sii)
              end
          | (_, _, _, Terminal a :: _, _, _) => (*scanner operation*)
              processS used States
                (S :: Si,
                  if Lexicon.matching_tokens (a, c) then movedot_term c S :: Sii else Sii)
          | (A, prec, ts, [], id, j) => (*completer operation*)
              let val tt = if id = "" then ts else [Node (id, rev ts)] in
                if j = i then (*lambda production?*)
                  let
                    val (prec', used') = update_trees (A, (tt, prec)) used;
                    val Slist = getS A prec prec' Si;
                    val States' = map (movedot_nonterm tt) Slist;
                  in processS used' (States' @ States) (S :: Si, Sii) end
                else
                  let val Slist = get_states Estate i j A prec
                  in processS used (map (movedot_nonterm tt) Slist @ States) (S :: Si, Sii) end
              end)
  in processS Inttab.empty states ([], []) end;


fun produce prods tags chains stateset i indata prev_token =
  (case Array.sub (stateset, i) of
    [] =>
      let
        val toks = if Lexicon.is_eof prev_token then indata else prev_token :: indata;
        val pos = Position.here (Lexicon.pos_of_token prev_token);
      in
        if null toks then
          error ("Inner syntax error: unexpected end of input" ^ pos)
        else
          error ("Inner syntax error" ^ pos ^
            Markup.markup Markup.no_report
              ("\n" ^ Pretty.string_of
                (Pretty.block [
                  Pretty.str "at", Pretty.brk 1,
                  Pretty.block
                   (Pretty.str "\"" ::
                    Pretty.breaks (map (Pretty.str o Lexicon.str_of_token) (#1 (split_last toks))) @
                    [Pretty.str "\""])])))
      end
  | s =>
      (case indata of
        [] => s
      | c :: cs =>
          let
            val (si, sii) = PROCESSS prods chains stateset i c s;
            val _ = Array.update (stateset, i, si);
            val _ = Array.update (stateset, i + 1, sii);
          in produce prods tags chains stateset (i + 1) cs c end));


fun get_trees states = map_filter (fn (_, _, [pt], _, _, _) => SOME pt | _ => NONE) states;

fun earley prods tags chains startsymbol indata =
  let
    val start_tag =
      (case Symtab.lookup tags startsymbol of
        SOME tag => tag
      | NONE => error ("Inner syntax: bad grammar root symbol " ^ quote startsymbol));
    val S0 = [(~1, 0, [], [Nonterminal (start_tag, 0), Terminal Lexicon.eof], "", 0)];
    val s = length indata + 1;
    val Estate = Array.array (s, []);
    val _ = Array.update (Estate, 0, S0);
  in
    get_trees (produce prods tags chains Estate 0 indata Lexicon.eof)
  end;


fun parse (Gram {tags, prods, chains, ...}) start toks =
  let
    val end_pos =
      (case try List.last toks of
        NONE => Position.none
      | SOME (Lexicon.Token (_, _, (_, end_pos))) => end_pos);
    val r =
      (case earley prods tags chains start (toks @ [Lexicon.mk_eof end_pos]) of
        [] => raise Fail "Inner syntax: no parse trees"
      | pts => pts);
  in r end;


fun guess_infix_lr (Gram gram) c = (*based on educated guess*)
  let
    fun freeze a = map_range (curry Vector.sub a) (Vector.length a);
    val prods = maps snd (maps snd (freeze (#prods gram)));
    fun guess (SOME ([Nonterminal (_, k),
            Terminal (Lexicon.Token (Lexicon.Literal, s, _)), Nonterminal (_, l)], _, j)) =
          if k = j andalso l = j + 1 then SOME (s, true, false, j)
          else if k = j + 1 then if l = j then SOME (s, false, true, j)
            else if l = j + 1 then SOME (s, false, false, j)
            else NONE
          else NONE
      | guess _ = NONE;
  in guess (find_first (fn (_, s, _) => s = c) prods) end;

end;
