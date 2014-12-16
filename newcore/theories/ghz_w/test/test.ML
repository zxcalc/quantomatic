local
  structure Theory = GHZW_Theory
  open Theory
  fun assert_g_eq msg exp actual =
    if Graph.exact_eq exp actual then ()
    else (writeln "Expected:";
          Graph.print exp;
          writeln "Actual:";
          Graph.print actual;
          raise ERROR (msg^": graphs differed"))
in
  val _ = Testing.test (theory_name^" theory save/restore") (fn () => let
    val g = Graph.empty
        |> Graph.add_named_vertex (V.mk "ghz") (Graph.NVert GHZW_Data.GHZ)
        |> Graph.add_named_vertex (V.mk "w") (Graph.NVert GHZW_Data.W)
        |> Graph.add_named_vertex (V.mk "tick") (Graph.NVert GHZW_Data.TICK)
        |> Graph.add_named_vertex (V.mk "zero") (Graph.NVert GHZW_Data.ZERO)
    val g' = GraphJSON.input (GraphJSON.output g)
    val _ = assert_g_eq "input (output g) = g" g g'
    val _ = "graphs/v1_format.graph"
          |> File_Io.read_json
          |> GraphJSON.input
          |> assert_g_eq "input v1_format.graph = g" g
    val _ = "graphs/v2_format.graph"
          |> File_Io.read_json
          |> GraphJSON.input
          |> assert_g_eq "input v2_format.graph = g" g
    (* if the file format changes, do
     * cp graphs/current_format.graph graphs/v<n>_format.graph
     * and add a new read test (see above) *)
    val _ = File_Io.write_json "graphs/current_format.graph" (GraphJSON.output g)
  in () end) ()
  val _ = Testing.assert_no_failed_tests()
end
