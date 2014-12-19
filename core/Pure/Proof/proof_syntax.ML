(*  Title:      Pure/Proof/proof_syntax.ML
    Author:     Stefan Berghofer, TU Muenchen

Function for parsing and printing proof terms.
*)

signature PROOF_SYNTAX =
sig
  val proofT: typ
  val add_proof_syntax: theory -> theory
  val proof_of_term: theory -> bool -> term -> Proofterm.proof
  val term_of_proof: Proofterm.proof -> term
  val cterm_of_proof: theory -> Proofterm.proof -> cterm * (cterm -> Proofterm.proof)
  val read_term: theory -> bool -> typ -> string -> term
  val read_proof: theory -> bool -> bool -> string -> Proofterm.proof
  val proof_syntax: Proofterm.proof -> theory -> theory
  val proof_of: bool -> thm -> Proofterm.proof
  val pretty_proof: Proof.context -> Proofterm.proof -> Pretty.T
  val pretty_proof_of: Proof.context -> bool -> thm -> Pretty.T
end;

structure Proof_Syntax : PROOF_SYNTAX =
struct

(**** add special syntax for embedding proof terms ****)

val proofT = Type ("proof", []);
val paramT = Type ("param", []);
val paramsT = Type ("params", []);
val idtT = Type ("idt", []);
val aT = TFree (Name.aT, []);

(** constants for theorems and axioms **)

fun add_proof_atom_consts names thy =
  thy
  |> Sign.root_path
  |> Sign.add_consts (map (fn name => (Binding.qualified_name name, proofT, NoSyn)) names);

(** constants for application and abstraction **)

fun add_proof_syntax thy =
  thy
  |> Sign.root_path
  |> Sign.set_defsort []
  |> Sign.add_types_global
    [(Binding.make ("proof", @{here}), 0, NoSyn)]
  |> fold (snd oo Sign.declare_const_global)
    [((Binding.make ("Appt", @{here}), [proofT, aT] ---> proofT), Mixfix ("(1_ %/ _)", [4, 5], 4)),
     ((Binding.make ("AppP", @{here}), [proofT, proofT] ---> proofT), Mixfix ("(1_ %%/ _)", [4, 5], 4)),
     ((Binding.make ("Abst", @{here}), (aT --> proofT) --> proofT), NoSyn),
     ((Binding.make ("AbsP", @{here}), [propT, proofT --> proofT] ---> proofT), NoSyn),
     ((Binding.make ("Hyp", @{here}), propT --> proofT), NoSyn),
     ((Binding.make ("Oracle", @{here}), propT --> proofT), NoSyn),
     ((Binding.make ("OfClass", @{here}), (Term.a_itselfT --> propT) --> proofT), NoSyn),
     ((Binding.make ("MinProof", @{here}), proofT), Delimfix "?")]
  |> Sign.add_nonterminals_global
    [Binding.make ("param", @{here}),
     Binding.make ("params", @{here})]
  |> Sign.add_syntax Syntax.mode_default
    [("_Lam", [paramsT, proofT] ---> proofT, Mixfix ("(1Lam _./ _)", [0, 3], 3)),
     ("_Lam0", [paramT, paramsT] ---> paramsT, Mixfix ("_/ _", [1, 0], 0)),
     ("_Lam0", [idtT, paramsT] ---> paramsT, Mixfix ("_/ _", [1, 0], 0)),
     ("_Lam1", [idtT, propT] ---> paramT, Mixfix ("_: _", [0, 0], 0)),
     ("", paramT --> paramT, Delimfix "'(_')"),
     ("", idtT --> paramsT, Delimfix "_"),
     ("", paramT --> paramsT, Delimfix "_")]
  |> Sign.add_syntax (Symbol.xsymbolsN, true)
    [("_Lam", [paramsT, proofT] ---> proofT, Mixfix ("(1\<^bold>\<lambda>_./ _)", [0, 3], 3)),
     (Lexicon.mark_const "Appt", [proofT, aT] ---> proofT, Mixfix ("(1_ \<cdot>/ _)", [4, 5], 4)),
     (Lexicon.mark_const "AppP", [proofT, proofT] ---> proofT, Mixfix ("(1_ \<bullet>/ _)", [4, 5], 4))]
  |> Sign.add_trrules (map Syntax.Parse_Print_Rule
    [(Ast.mk_appl (Ast.Constant "_Lam")
        [Ast.mk_appl (Ast.Constant "_Lam0")
          [Ast.Variable "l", Ast.Variable "m"], Ast.Variable "A"],
      Ast.mk_appl (Ast.Constant "_Lam")
        [Ast.Variable "l",
          Ast.mk_appl (Ast.Constant "_Lam") [Ast.Variable "m", Ast.Variable "A"]]),
     (Ast.mk_appl (Ast.Constant "_Lam")
        [Ast.mk_appl (Ast.Constant "_Lam1")
          [Ast.Variable "x", Ast.Variable "A"], Ast.Variable "B"],
      Ast.mk_appl (Ast.Constant (Lexicon.mark_const "AbsP")) [Ast.Variable "A",
        (Ast.mk_appl (Ast.Constant "_abs") [Ast.Variable "x", Ast.Variable "B"])]),
     (Ast.mk_appl (Ast.Constant "_Lam") [Ast.Variable "x", Ast.Variable "A"],
      Ast.mk_appl (Ast.Constant (Lexicon.mark_const "Abst"))
        [(Ast.mk_appl (Ast.Constant "_abs") [Ast.Variable "x", Ast.Variable "A"])])]);


(**** translation between proof terms and pure terms ****)

fun proof_of_term thy ty =
  let
    val thms = Global_Theory.all_thms_of thy true;
    val axms = Theory.all_axioms_of thy;

    fun mk_term t = (if ty then I else map_types (K dummyT))
      (Term.no_dummy_patterns t);

    fun prf_of [] (Bound i) = PBound i
      | prf_of Ts (Const (s, Type ("proof", _))) =
          Proofterm.change_type (if ty then SOME Ts else NONE)
            (case Long_Name.explode s of
               "axm" :: xs =>
                 let
                   val name = Long_Name.implode xs;
                   val prop = (case AList.lookup (op =) axms name of
                       SOME prop => prop
                     | NONE => error ("Unknown axiom " ^ quote name))
                 in PAxm (name, prop, NONE) end
             | "thm" :: xs =>
                 let val name = Long_Name.implode xs;
                 in (case AList.lookup (op =) thms name of
                     SOME thm =>
                      fst (Proofterm.strip_combt (fst (Proofterm.strip_combP (Thm.proof_of thm))))
                   | NONE => error ("Unknown theorem " ^ quote name))
                 end
             | _ => error ("Illegal proof constant name: " ^ quote s))
      | prf_of Ts (Const ("OfClass", _) $ Const (c_class, _)) =
          (case try Logic.class_of_const c_class of
            SOME c =>
              Proofterm.change_type (if ty then SOME Ts else NONE)
                (OfClass (TVar ((Name.aT, 0), []), c))
          | NONE => error ("Bad class constant: " ^ quote c_class))
      | prf_of Ts (Const ("Hyp", _) $ prop) = Hyp prop
      | prf_of Ts (v as Var ((_, Type ("proof", _)))) = Hyp v
      | prf_of [] (Const ("Abst", _) $ Abs (s, T, prf)) =
          if T = proofT then
            error ("Term variable abstraction may not bind proof variable " ^ quote s)
          else Abst (s, if ty then SOME T else NONE,
            Proofterm.incr_pboundvars (~1) 0 (prf_of [] prf))
      | prf_of [] (Const ("AbsP", _) $ t $ Abs (s, _, prf)) =
          AbsP (s, case t of
                Const ("Pure.dummy_pattern", _) => NONE
              | _ $ Const ("Pure.dummy_pattern", _) => NONE
              | _ => SOME (mk_term t),
            Proofterm.incr_pboundvars 0 (~1) (prf_of [] prf))
      | prf_of [] (Const ("AppP", _) $ prf1 $ prf2) =
          prf_of [] prf1 %% prf_of [] prf2
      | prf_of Ts (Const ("Appt", _) $ prf $ Const ("Pure.type", Type (_, [T]))) =
          prf_of (T::Ts) prf
      | prf_of [] (Const ("Appt", _) $ prf $ t) = prf_of [] prf %
          (case t of Const ("Pure.dummy_pattern", _) => NONE | _ => SOME (mk_term t))
      | prf_of _ t = error ("Not a proof term:\n" ^
          Syntax.string_of_term_global thy t)

  in prf_of [] end;


val AbsPt = Const ("AbsP", [propT, proofT --> proofT] ---> proofT);
val AppPt = Const ("AppP", [proofT, proofT] ---> proofT);
val Hypt = Const ("Hyp", propT --> proofT);
val Oraclet = Const ("Oracle", propT --> proofT);
val OfClasst = Const ("OfClass", (Term.itselfT dummyT --> propT) --> proofT);
val MinProoft = Const ("MinProof", proofT);

val mk_tyapp = fold (fn T => fn prf => Const ("Appt",
  [proofT, Term.itselfT T] ---> proofT) $ prf $ Logic.mk_type T);

fun term_of _ (PThm (_, ((name, _, NONE), _))) =
      Const (Long_Name.append "thm" name, proofT)
  | term_of _ (PThm (_, ((name, _, SOME Ts), _))) =
      mk_tyapp Ts (Const (Long_Name.append "thm" name, proofT))
  | term_of _ (PAxm (name, _, NONE)) = Const (Long_Name.append "axm" name, proofT)
  | term_of _ (PAxm (name, _, SOME Ts)) =
      mk_tyapp Ts (Const (Long_Name.append "axm" name, proofT))
  | term_of _ (OfClass (T, c)) =
      mk_tyapp [T] (OfClasst $ Const (Logic.const_of_class c, Term.itselfT dummyT --> propT))
  | term_of _ (PBound i) = Bound i
  | term_of Ts (Abst (s, opT, prf)) =
      let val T = the_default dummyT opT
      in Const ("Abst", (T --> proofT) --> proofT) $
        Abs (s, T, term_of (T::Ts) (Proofterm.incr_pboundvars 1 0 prf))
      end
  | term_of Ts (AbsP (s, t, prf)) =
      AbsPt $ the_default Term.dummy_prop t $
        Abs (s, proofT, term_of (proofT::Ts) (Proofterm.incr_pboundvars 0 1 prf))
  | term_of Ts (prf1 %% prf2) =
      AppPt $ term_of Ts prf1 $ term_of Ts prf2
  | term_of Ts (prf % opt) =
      let val t = the_default Term.dummy opt
      in Const ("Appt",
        [proofT, fastype_of1 (Ts, t) handle TERM _ => dummyT] ---> proofT) $
          term_of Ts prf $ t
      end
  | term_of Ts (Hyp t) = Hypt $ t
  | term_of Ts (Oracle (_, t, _)) = Oraclet $ t
  | term_of Ts MinProof = MinProoft;

val term_of_proof = term_of [];

fun cterm_of_proof thy prf =
  let
    val thm_names = map fst (Global_Theory.all_thms_of thy true);
    val axm_names = map fst (Theory.all_axioms_of thy);
    val thy' = thy
      |> add_proof_syntax
      |> add_proof_atom_consts
        (map (Long_Name.append "axm") axm_names @ map (Long_Name.append "thm") thm_names);
  in
    (cterm_of thy' (term_of_proof prf), proof_of_term thy true o Thm.term_of)
  end;

fun read_term thy topsort =
  let
    val thm_names = filter_out (fn s => s = "") (map fst (Global_Theory.all_thms_of thy true));
    val axm_names = map fst (Theory.all_axioms_of thy);
    val ctxt = thy
      |> add_proof_syntax
      |> add_proof_atom_consts
        (map (Long_Name.append "axm") axm_names @ map (Long_Name.append "thm") thm_names)
      |> Proof_Context.init_global
      |> Proof_Context.allow_dummies
      |> Proof_Context.set_mode Proof_Context.mode_schematic
      |> topsort ? (Config.put Type_Infer_Context.const_sorts false #> Proof_Context.set_defsort []);
  in
    fn ty => fn s =>
      (if ty = propT then Syntax.parse_prop else Syntax.parse_term) ctxt s
      |> Type.constraint ty |> Syntax.check_term ctxt
  end;

fun read_proof thy topsort =
  let val rd = read_term thy topsort proofT
  in fn ty => fn s => proof_of_term thy ty (Logic.varify_global (rd s)) end;

fun proof_syntax prf =
  let
    val thm_names = Symtab.keys (Proofterm.fold_proof_atoms true
      (fn PThm (_, ((name, _, _), _)) => if name <> "" then Symtab.update (name, ()) else I
        | _ => I) [prf] Symtab.empty);
    val axm_names = Symtab.keys (Proofterm.fold_proof_atoms true
      (fn PAxm (name, _, _) => Symtab.update (name, ()) | _ => I) [prf] Symtab.empty);
  in
    add_proof_syntax #>
    add_proof_atom_consts
      (map (Long_Name.append "thm") thm_names @ map (Long_Name.append "axm") axm_names)
  end;

fun proof_of full thm =
  let
    val thy = Thm.theory_of_thm thm;
    val prop = Thm.full_prop_of thm;
    val prf = Thm.proof_of thm;
    val prf' =
      (case Proofterm.strip_combt (fst (Proofterm.strip_combP prf)) of
        (PThm (_, ((_, prop', _), body)), _) =>
          if prop = prop' then Proofterm.join_proof body else prf
      | _ => prf)
  in if full then Reconstruct.reconstruct_proof thy prop prf' else prf' end;

fun pretty_proof ctxt prf =
  Proof_Context.pretty_term_abbrev
    (Proof_Context.transfer (proof_syntax prf (Proof_Context.theory_of ctxt)) ctxt)
    (term_of_proof prf);

fun pretty_proof_of ctxt full th =
  pretty_proof ctxt (proof_of full th);

end;
