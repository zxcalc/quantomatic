

structure Substrings_Theory = GraphicalTheory(
  structure Data = Substrings_Data
  structure DataIO = Substrings_ComponentDataIO)


(* Use this for convenience if you don't need annotations *)
structure Substrings_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = Substrings_Theory
  structure GraphComponentDataIO = Substrings_ComponentDataIO
)
