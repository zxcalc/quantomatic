signature JSON_CONTROLLER =
sig
  structure Theory : GRAPHICAL_THEORY
  val dispatch : (string * string) * Json.json -> Json.json
end


functor JsonController(Theory : GRAPHICAL_THEORY) =
struct

open JsonControllerUtil

structure Theory = Theory
structure CModTest = CModTest(Theory)

val mtab = Symtab.make [
  ("test", (CModTest.ftab, "Utility functions"))
]

end
