signature CONTROLLER =
sig
  type functiontab = ((Json.json -> Json.json) * string) Symtab.table
  val mtab : (functiontab * string) Symtab.table
end


functor Controller(Theory : GRAPHICAL_THEORY) =
struct

type functiontab = ((Json.json -> Json.json) * string) Symtab.table

structure CModMain = CModMain(Theory)


val mtab = Symtab.make [
  ("Main", (CModMain.ftab, "Utility functions"))
]

end
