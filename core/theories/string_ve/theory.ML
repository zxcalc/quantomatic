(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
(*  String Vertex/Edge Theory: both verts and edges have string data *)
(* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- *) 
structure StringVE_Theory = GraphicalTheory(
  structure Data = StringVE_Data
  structure DataIO = StringVE_ComponentDataIO
)

(* OLD I/O structure *)
(*structure StringVE_IO = GraphicalTheoryIO(
  structure Theory = StringVE_Theory
  structure GraphComponentDataIO = StringVE_ComponentDataIO
)*)
