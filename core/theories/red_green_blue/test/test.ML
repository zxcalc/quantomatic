local
  structure IO = RGB_GraphicalTheoryIO
  open IO.Theory
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
        |> Graph.add_named_vertex (V.mk "r") (Graph.NVert RGB_Data.Red)
        |> Graph.add_named_vertex (V.mk "g") (Graph.NVert RGB_Data.Green)
        |> Graph.add_named_vertex (V.mk "b") (Graph.NVert RGB_Data.Blue)
        |> Graph.add_named_vertex (V.mk "w") Graph.WVert
        |> Graph.add_named_edge (E.mk "e1") (Directed,()) (V.mk "r") (V.mk "w")
        |> Graph.add_named_edge (E.mk "e2") (Directed,()) (V.mk "w") (V.mk "g")
    val g' = IO.InputGraphJSON.input (IO.OutputGraphJSON.output g)
    val _ = assert_g_eq "input (output g) = g" g g'
    val _ = "graphs/v1_format.graph"
          |> File_Io.read_json
          |> IO.InputGraphJSON.input
          |> assert_g_eq "input v1_format.graph = g" g
    (* if the file format changes, do
     * cp graphs/current_format.graph graphs/v<n>_format.graph
     * and add a new read test (see above) *)
    val _ = File_Io.write_json "graphs/current_format.graph" (IO.OutputGraphJSON.output g)
  in () end) ()
  val _ = Testing.assert_no_failed_tests()
end
