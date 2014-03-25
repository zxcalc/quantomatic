infixr 4 ++

signature SIMP_UTIL =
sig
  structure Theory : GRAPHICAL_THEORY
  type dstep = (R.name * Theory.Rule.T) * Theory.Graph.T
  type simproc = Theory.Graph.T -> dstep Seq.seq
  val load_rule : string -> R.name * Theory.Rule.T
  (*val load_graph : string -> Theory.Graph.T*)
  val ruleset : (R.name * Theory.Rule.T) list -> Theory.Ruleset.T
  val REDUCE_ALL : Theory.Ruleset.T -> simproc
  val REDUCE : (R.name * Theory.Rule.T) -> simproc
  val REDUCE_WITH : { PRE  : Theory.Graph.T -> bool ,
                      POST : Theory.Graph.T * Theory.Graph.T -> bool }
                    -> (R.name * Theory.Rule.T)
                    -> simproc

  val REDUCE_METRIC : (Theory.Graph.T -> int)
                      -> (R.name * Theory.Rule.T)
                      -> simproc

  val REDUCE_METRIC_TO : int -> (Theory.Graph.T -> int)
                         -> (R.name * Theory.Rule.T)
                         -> simproc

  val ++ : simproc * simproc -> simproc
  val LOOP : simproc -> simproc


  val derive : simproc -> Theory.Graph.T -> Json.json
end

functor SimpUtil(
  structure Theory : GRAPHICAL_THEORY
  structure IO : GRAPH_COMPONENT_DATA_IO
  sharing type Theory.Graph.nvdata = IO.nvdata
  sharing type Theory.Graph.edata = IO.edata
) : SIMP_UTIL = 
struct

structure Theory = Theory
type dstep = (R.name * Theory.Rule.T) * Theory.Graph.T
type simproc = Theory.Graph.T -> dstep Seq.seq

structure JsonIO = JsonObjectAnnotatedGraphicalTheoryIO(
  structure Theory = Theory
  structure GraphComponentDataIO = IO)

fun load_rule s = (R.mk s, fst (JsonIO.InputRuleJSON.input (Json.read_file (s^".qrule"))))

(*fun load_graph s = fst (JsonIO.InputGraphJSON.input (Json.read_file (s^".qgraph")))*)

fun ruleset rule_list = fold (fn r => fn rs => (
    rs |> Theory.Ruleset.update_rule r
       |> Theory.Ruleset.activate_rule (fst r)
  )) rule_list Theory.Ruleset.empty

fun REDUCE_ALL ruleset graph =
  Seq.make (fn () => (
    case Seq.pull (Theory.RulesetRewriter.apply ruleset graph)
      of SOME ((r,g'), _) => SOME ((r,g'), REDUCE_ALL ruleset g')
       | NONE => NONE
  ))

fun REDUCE rule graph =
  Seq.make (fn () => (
    case Seq.pull (Theory.Rewriter.find_rewrites (snd rule) graph)
      of SOME ((r,g'), _) => SOME (((fst rule, r),g'), REDUCE rule g')
       | NONE => NONE
  ))

fun REDUCE_WITH {PRE=PRE, POST=POST} rule graph =
  Seq.make (fn () => (
    if not (PRE graph) then NONE
    else case Seq.pull (Seq.filter
                       (fn (_, g') => POST (graph, g'))
                       (Theory.Rewriter.find_rewrites (snd rule) graph))
           of SOME ((r,g'), _) => SOME (((fst rule, r),g'), REDUCE_WITH {PRE=PRE,POST=POST} rule g')
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

(*fun derivation g simproc =*)

fun derive simp g = let
  val steps = Seq.list_of (simp g)
in
  Json.Object (fold_index (fn (i,((rname,rule),graph)) =>
      Json.update (
        "s" ^ Int.toString i,
        Json.mk_object [
          ("parent", if i = 0 then Json.Null else Json.String ("s" ^ Int.toString (i - 1))),
          ("rule_name", Json.String (R.dest rname))(*,
          ("rule", JsonIO.OutputRuleJSON.output
                   (rule,JsonIO.OutputRuleJSON.OA.empty_annotation))*)
        ]
      ))
    steps Json.empty_obj)
end

(*fun save_derivation seq = *)

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
