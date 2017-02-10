(*------------------------------------------------------------------------------
This file demonstrates some handy bits of ML for playing with graphs in Quantomatic.
------------------------------------------------------------------------------*)

local
structure Theory = RTechn_Theory
open Theory
in
val g = "../theories/isaplanner_rtechn/test/graphs/rippling.graph"
  |> Json.read_file
  |> GraphJSON.input
end
