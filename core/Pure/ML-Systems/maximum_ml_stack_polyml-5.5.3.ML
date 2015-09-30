(*  Title:      Pure/ML-Systems/maximum_ml_stack_polyml-5.5.3.ML

Maximum stack size (in words) for ML threads -- Poly/ML 5.5.3, or later.
*)

fun maximum_ml_stack limit = [Thread.MaximumMLStack limit];

