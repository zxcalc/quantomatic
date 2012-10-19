type cmod_functiontab = ((Json.json -> string * Json.json) * string) Symtab.table

signature CONTROLLER_MODULE =
sig
  (* a table mapping function name to function * docstring *)
  val ftab : cmod_functiontab
end
