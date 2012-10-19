type controller_moduletab = (cmod_functiontab * string) Symtab.table

signature CONTROLLER =
sig
  structure Theory
  val mtab : controller_moduletab
end


functor Controller(Theory : GRAPHICAL_THEORY) =
struct

structure Theory = Theory
structure CModMain = CModMain(Theory)


val mtab = Symtab.make [
  ("Main", (CModMain.ftab, "Utility functions"))
]

end
