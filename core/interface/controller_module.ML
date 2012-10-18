signature CONTROLLER_MODULE =
sig
  (* a table mapping function name to function * docstring *)
  type functiontab = ((Json.json -> Json.json) * string) Symtab.table
  val mk : unit -> functiontab
end
