infixr 4 ++

signature SYNTH_UTIL =
sig
  structure Theory : GRAPHICAL_THEORY

  val load_rule : string -> (R.name * Theory.Rule.T)
  val load_ruleset : string list -> Theory.Ruleset.T
  val load_graph : string -> Theory.Graph.T
  val save_graph : string -> Theory.Graph.T -> unit
end

functor SynthUtil(
  structure Enum : GRAPH_ENUM
) = 
struct

structure Theory = Enum.Theory

fun esc s =
  let
    fun f #"/" = "--"
      | f c = String.str c
    in String.translate f s
  end

fun load_rule s = (R.mk s, Theory.RuleJSON.input (Json.read_file (s^".qrule")))
fun save_rule s r =  Json.write_file (s^".qrule") (Theory.RuleJSON.output r)
fun load_graph s = Theory.GraphJSON.input (Json.read_file (s^".qgraph"))
fun save_graph s g = Json.write_file (s^".qgraph") (Theory.GraphJSON.output g)

fun ruleset rule_list = fold (fn r => fn rs => (
    rs |> Theory.Ruleset.update_rule r
       |> Theory.Ruleset.activate_rule (fst r)
  )) rule_list Theory.Ruleset.empty

fun load_ruleset rule_list = ruleset (map load_rule rule_list)

fun save_ruleset output_dir rs =
  RTab.fold
  (fn (rn,r) => fn () =>
    (save_rule (output_dir ^ "/" ^ esc (R.dest rn)) r))
  (Theory.Ruleset.get_allrules rs) ()


fun mk_output_dir () = let
  val _ = if not (OS.FileSys.access ("synth_output", []))
          then OS.FileSys.mkDir "synth_output"
          else ()
  fun frdir i = let
    val dir = "synth_output/s" ^ (Int.toString i)
  in
    if not (OS.FileSys.access (dir, []))
    then (OS.FileSys.mkDir dir; dir)
    else frdir (i+1)
  end
in frdir 0
end

(*fun synth gens rs0 (sz as (i,j,k,l)) = let
  val _ = TextIO.print ("Starting enumeration for ("^
            Int.toString i ^ "," ^ Int.toString j ^ "," ^
            Int.toString k ^ "," ^ Int.toString l ^
          ")\n")
  val tab = Enum.EqClassTab.empty |> Enum.EqClassTab.set_initial_rs rs0
  val tab = tab |> Enum.tab_update gens sz
  val _ = TextIO.print "done\n"
  val rs = Enum.EqClassTab.get_ruleset tab
  val dir = mk_output_dir ()
  val _ = TextIO.print ("saving output to: " ^ dir ^ "\n")
in
  save_ruleset dir rs
end*)

fun synth_run gens (sz as (i,j,k,l)) tab = let
  val _ = TextIO.print ("Starting enumeration run for ("^
            Int.toString i ^ "," ^ Int.toString j ^ "," ^
            Int.toString k ^ "," ^ Int.toString l ^
          ")\n")
  val tab = tab |> Enum.tab_update gens sz
  val _ = TextIO.print "done\n"
in
  tab
end

fun synth gens rs0 runs = let
  val _ = TextIO.print ("Initialising synthesis\n")
  val tab = Enum.EqClassTab.empty |> Enum.EqClassTab.set_initial_rs rs0
  val _ = TextIO.print "done\n"
  val tab = fold (synth_run gens) runs tab
  val rs = Enum.EqClassTab.get_ruleset tab
  val dir = mk_output_dir ()
  val _ = TextIO.print ("saving output to: " ^ dir ^ "\n")
in
  save_ruleset dir rs
end


end
