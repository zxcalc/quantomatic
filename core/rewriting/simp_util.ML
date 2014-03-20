infixr 4 ++

functor SimpUtil(
  structure Theory : GRAPHICAL_THEORY
  structure IO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = IO.nvdata
  sharing type Theory.Graph.edata = IO.edata
) = 
struct

structure JsonIO = JsonObjectAnnotatedGraphicalTheoryIO(
  structure Theory = Theory
  structure GraphComponentDataIO = IO)

fun load_rule s = fst (JsonIO.InputRuleJSON.input (Json.read_file (s^".qrule")))

fun ruleset rule_list = fold (fn r => fn rs => (
    let
      val (rname, rs') = Theory.Ruleset.add_fresh_rule r rs
    in Theory.Ruleset.activate_rule rname rs'
    end
  )) rule_list Theory.Ruleset.empty

fun REDUCE_ALL ruleset graph =
  Seq.make (fn () => (
    case Seq.pull (Theory.RulesetRewriter.apply ruleset graph)
      of SOME (((_,r),g'), _) => SOME ((r,g'), REDUCE_ALL ruleset g')
       | NONE => NONE
  ))

fun REDUCE rule graph =
  Seq.make (fn () => (
    case Seq.pull (Theory.Rewriter.find_rewrites rule graph)
      of SOME ((r,g'), _) => SOME ((r,g'), REDUCE rule g')
       | NONE => NONE
  ))

fun REDUCE_WITH {PRE=PRE, POST=POST} rule graph =
  Seq.make (fn () => (
    if not (PRE graph) then NONE
    else case Seq.pull (Seq.filter
                       (fn (_, g') => POST (graph, g'))
                       (Theory.Rewriter.find_rewrites rule graph))
           of SOME ((r,g'), _) => SOME ((r,g'), REDUCE_WITH {PRE=PRE,POST=POST} rule g')
            | NONE => NONE
  ))


(*fun REDUCE_METRIC metric rule graph =
  Seq.make (fn () => (
    let
      val m = metric graph
    in
      case Seq.pull (Seq.filter
                      (fn (_, g') => metric g' < m)
                      (Theory.Rewriter.find_rewrites rule graph))
        of SOME ((r,g'), _) => SOME ((r,g'), REDUCE_METRIC metric rule g')
         | NONE => NONE
    end
  ))*)


fun REDUCE_METRIC_TO i metric =
  REDUCE_WITH { PRE  = (fn g => (i <= metric g)),
                POST = (fn (g,g') => (metric g' < metric g)) }

val REDUCE_METRIC = REDUCE_METRIC_TO 0

(*fun REDUCE_METRIC_TO i metric rule graph =
  Seq.make (fn () => (
    let
      val m = metric graph
    in
      if m <= i then NONE
      else case Seq.pull (Seq.filter
                         (fn (_, g') => metric g' < m)
                         (Theory.Rewriter.find_rewrites rule graph))
             of SOME ((r,g'), _) => SOME ((r,g'), REDUCE_METRIC_TO i metric rule g')
              | NONE => NONE
    end
  ))*)

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

fun LOOP seqf graph =
  Seq.make (fn () => (
    case Seq.pull (seqf graph)
      of SOME ((r,g'), s1) => SOME ((r,g'), g' |> ((fn _ => s1) ++ (LOOP seqf)))
       | NONE => NONE
  ))

end
