infixr 4 ++

signature SIMP_UTIL =
sig
  structure Theory : GRAPHICAL_THEORY
  type dstep = (R.name * Theory.Rule.T) * Theory.Graph.T
  type simproc = Theory.Graph.T -> dstep Seq.seq
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

  val normalise : simproc -> Theory.Graph.T -> Theory.Graph.T option


  val json_of_derivation : simproc -> Theory.Graph.T -> Json.json
  val save_derivation : string -> simproc -> Theory.Graph.T -> unit
  val load_rule : string -> (R.name * Theory.Rule.T)
  val load_graph : string -> Theory.Graph.T
  val save_graph : string -> Theory.Graph.T -> unit
end

functor SimpUtil(
  structure Theory : GRAPHICAL_THEORY
) : SIMP_UTIL = 
struct

structure Theory = Theory
type dstep = (R.name * Theory.Rule.T) * Theory.Graph.T
type simproc = Theory.Graph.T -> dstep Seq.seq


fun load_rule s = (R.mk s, Theory.RuleJSON.input (Json.read_file (s^".qrule")))
fun load_graph s = Theory.GraphJSON.input (Json.read_file (s^".qgraph"))
fun save_graph s g = Json.write_file (s^".qgraph") (Theory.GraphJSON.output g)

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

fun json_of_derivation simp g = let
  val steps = Seq.list_of (simp g)
in
  Json.mk_object [
    ("root", Theory.GraphJSON.output g),
    ("steps",
        Json.Object (fold_index (fn (i,((rname,rule),graph)) =>
            Json.update (
              "s" ^ Int.toString i,
              Json.mk_object [
                ("parent", if i = 0 then Json.Null else Json.String ("s" ^ Int.toString (i - 1))),
                ("rule_name", Json.String (R.dest rname)),
                ("rule", Theory.RuleJSON.output rule),
                ("graph", Theory.GraphJSON.output graph)
              ]
            ))
          steps Json.empty_obj)),
    ("heads", Json.Array [Json.String ("s" ^ Int.toString (length steps - 1))])
  ]
end

fun save_derivation s simp g = Json.write_file (s^".qderive") (json_of_derivation simp g)

fun normalise simp g = let
  val seq = (simp g)
  fun norm prev seq = case Seq.pull seq
                        of SOME ((_,g), seq') => norm (SOME g) seq'
                         | NONE => prev
in norm NONE seq
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
