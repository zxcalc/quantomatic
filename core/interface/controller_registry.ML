
structure RG_Controller = Controller(
  structure Theory = RG_Theory
  structure GraphComponentDataIO = RG_ComponentDataIO
);

(*
(*TODO: re-implement these in the new format*)
structure GHZW_Controller = Controller(
  structure Theory = GHZW_Theory
  structure GraphComponentDataIO = GHZW_ComponentDataIO
);
structure Petri_Controller = Controller(
  structure Theory = Petri_Theory
  structure GraphComponentDataIO = Petri_ComponentDataIO
);
structure RGB_Controller = Controller(
  structure Theory = RGB_Theory
  structure GraphComponentDataIO = RGB_ComponentDataIO
);
structure SUBSTR_LINRAT_Controller = Controller(
  structure Theory = SUBSTR_LINRAT_Theory
  structure GraphComponentDataIO = SUBSTR_LINRAT_ComponentDataIO
);
structure Str_Controller = Controller(
  structure Theory = Str_Theory
  structure GraphComponentDataIO = Str_ComponentDataIO
);
structure RTechn_Controller = Controller(
  structure Theory = RTechn_Theory
  structure GraphComponentDataIO = RTechn_ComponentDataIO
);*)

signature CONTROLLER_REGISTRY
= sig
  type T;
  val empty : T;
  val default : T;
  val register_eval_func : string -> (Commands.request -> Commands.response) -> T -> T;
  val get_eval_func : T -> string -> (Commands.request -> Commands.response) option;
  val get_theories : T -> string list;
end;

structure ControllerRegistry : CONTROLLER_REGISTRY
= struct
  type T = (Commands.request -> Commands.response) Symtab.table;

  val empty = Symtab.empty;

  fun register_eval_func theory_name func reg =
        Symtab.update_new (theory_name,func) reg
        handle Symtab.DUP _ => 
          raise ERROR ("Cannot register theory: '" ^ theory_name 
            ^ "'; this theory is already registered.");

  val get_eval_func = Symtab.lookup

  val get_theories = Symtab.keys

  val default =
    Symtab.empty |> (register_eval_func
                      RG_Controller.theoryName
                      RG_Controller.serviceRequest)
                 (*|> (register_eval_func
                      GHZW_Controller.theoryName
                      GHZW_Controller.serviceRequest)
                 |> (register_eval_func
                      Petri_Controller.theoryName
                      Petri_Controller.serviceRequest)
                 |> (register_eval_func
                      RGB_Controller.theoryName
                      RGB_Controller.serviceRequest)
                 |> (register_eval_func
                      SUBSTR_LINRAT_Controller.theoryName
                      SUBSTR_LINRAT_Controller.serviceRequest)
                 |> (register_eval_func
                      Str_Controller.theoryName
                      Str_Controller.serviceRequest)
                 |> (register_eval_func
                      RTechn_Controller.theoryName
                      RTechn_Controller.serviceRequest)*)
end;


