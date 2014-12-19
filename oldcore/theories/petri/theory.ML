

structure Petri_Theory = GraphicalTheory(
  structure Data = Petri_Data
  structure DataIO = Petri_ComponentDataIO)


(* Use this for convenience if you don't need annotations *)
(*structure Petri_GraphicalTheoryIO = GraphicalTheoryIO(
  structure Theory = Petri_Theory
  structure GraphComponentDataIO = Petri_ComponentDataIO
)*)
