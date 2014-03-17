PolyML.SaveState.loadState "../heaps/quanto.heap";
PolyML.Compiler.printDepth:=100;

structure G = RG_Theory.Graph;

structure IO = JsonObjectAnnotatedGraphicalTheoryIO(
  structure Theory = RG_Theory
  structure GraphComponentDataIO = RG_ComponentDataIO)


val (sample, gann) = IO.InputGraphJSON.input (Json.read_file "sample2.qgraph");

fun load_rule s = fst (IO.InputRuleJSON.input (Json.read_file (s^".qrule")))
fun save_graph file g = Json.write_file file (IO.OutputGraphJSON.output (g, gann))

fun ruleset rule_list = fold (fn r => fn rs => (
    let
      val (rname, rs') = RG_Theory.Ruleset.add_fresh_rule r rs
    in RG_Theory.Ruleset.activate_rule rname rs'
    end
  )) rule_list RG_Theory.Ruleset.empty

fun REDUCE_ALL ruleset graph =
  Seq.make (fn () => (
    case Seq.pull (RG_Theory.RulesetRewriter.apply ruleset graph)
      of SOME (((_,r),g'), _) => SOME ((r,g'), REDUCE_ALL ruleset g')
       | NONE => NONE
  ))

fun REDUCE rule graph =
  Seq.make (fn () => (
    case Seq.pull (RG_Theory.Rewriter.find_rewrites rule graph)
      of SOME ((r,g'), _) => SOME ((r,g'), REDUCE rule g')
       | NONE => NONE
  ))

fun REDUCE_METRIC metric rule graph =
  Seq.make (fn () => (
    let
      val m = metric graph
    in
      case Seq.pull (Seq.filter
                      (fn (_, g') => metric g' < m)
                      (RG_Theory.Rewriter.find_rewrites rule graph))
        of SOME ((r,g'), _) => SOME ((r,g'), REDUCE_METRIC metric rule g')
         | NONE => NONE
    end
  ))

fun REDUCE_METRIC_TO i metric rule graph =
  Seq.make (fn () => (
    let
      val m = metric graph
    in
      if m <= i then NONE
      else case Seq.pull (Seq.filter
                         (fn (_, g') => metric g' < m)
                         (RG_Theory.Rewriter.find_rewrites rule graph))
             of SOME ((r,g'), _) => SOME ((r,g'), REDUCE_METRIC_TO i metric rule g')
              | NONE => NONE
    end
  ))

fun final seq = let
  fun fin prev seq = case Seq.pull seq
                     of SOME (v, seq') => fin (SOME v) seq'
                      | NONE => prev
in fin NONE seq
end

(* produces a rewrite sequence by feeding the last graph in seqf1 to seqf2 *)
fun op++ (seqf1, seqf2) graph = let
  fun chain prev_graph s1 =
    Seq.make (fn () => (
      case Seq.pull s1
        of SOME ((r,g'), s1') => SOME ((r,g'), chain (SOME g') s1')
         | NONE => Seq.pull (seqf2 (case prev_graph of SOME g' => g' | NONE => graph))
    ))
in chain NONE (seqf1 graph)
end

infixr 4 ++

fun LOOP seqf graph =
  Seq.make (fn () => (
    case Seq.pull (seqf graph)
      of SOME ((r,g'), s1) => SOME ((r,g'), g' |> ((fn _ => s1) ++ (LOOP seqf)))
       | NONE => NONE
  ))


(* actual simproc starts here... *)

fun is_red g v = case G.get_vertex_data g v
                   of (G.NVert (RG_Data.Xnd _)) => true | _ => false

fun is_green g v = case G.get_vertex_data g v
                     of (G.NVert (RG_Data.Znd _)) => true | _ => false

fun is_boundary_red g v = (G.is_boundary g v) andalso V.NSet.exists (is_red g) (G.get_adj_vertices g v)

fun num_boundary_red g = let
  val gmin = G.minimise g
in V.NSet.cardinality (V.NSet.filter (is_boundary_red gmin) (G.get_vertices gmin))
end

fun is_interior_green g v = (is_green g v) andalso V.NSet.forall (is_red g) (G.get_adj_vertices g v)

fun min_green_arity g = let
  fun min v (SOME a) = SOME (Int.min (Arity.get_undir (G.get_arity g v), a))
    | min v NONE = SOME (Arity.get_undir (G.get_arity g v))
in
  case V.NSet.fold min (V.NSet.filter (is_interior_green g) (G.get_vertices g)) NONE
    of SOME a => a
     | NONE => 0
end

val rotate = load_rule "rules/rotate"
val green_ids = load_rule "rules/green_ids"
val green_elim = load_rule "rules/green_elim"

val red_copy = load_rule "rules/red_copy"
val red_sp = load_rule "rules/red_sp"
val green_sp = load_rule "rules/green_sp"
val green_scalar = load_rule "rules/green_scalar"
val red_scalar = load_rule "rules/red_scalar"
val hopf = load_rule "rules/hopf"

val simps = ruleset [red_copy, red_sp, green_sp, hopf, red_scalar, green_scalar]

fun simproc g = g |> (
  REDUCE_ALL simps ++
  REDUCE_METRIC num_boundary_red green_ids ++
  LOOP (
    REDUCE_METRIC_TO 1 min_green_arity rotate ++
    REDUCE green_elim
  ) ++
  REDUCE_ALL simps
)

val SOME (_, g') = final (simproc sample)

val _ = save_graph "test.qgraph" g'


