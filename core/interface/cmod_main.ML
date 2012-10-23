signature CMOD_MAIN =
sig
  include CONTROLLER_MODULE
end

functor CModMain(Theory : GRAPHICAL_THEORY) : CMOD_MAIN =
struct
  fun echo x = x

  val ftab = Symtab.make [
    ("echo", (echo, "Echoes the provided input."))
  ]
end
