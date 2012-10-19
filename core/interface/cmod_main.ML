signature CMOD_MAIN =
sig
  include CONTROLLER_MODULE
end

functor CModMain(Theory : GRAPHICAL_THEORY) : CMOD_MAIN =
struct
  val ftab = Symtab.make [
    (* *)
  ]
end
