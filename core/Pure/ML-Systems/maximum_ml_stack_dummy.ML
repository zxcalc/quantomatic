(*  Title:      Pure/ML-Systems/maximum_ml_stack_dummy.ML

Maximum stack size (in words) for ML threads -- dummy version.
*)

fun maximum_ml_stack (_: int option) : Thread.threadAttribute list = [];

