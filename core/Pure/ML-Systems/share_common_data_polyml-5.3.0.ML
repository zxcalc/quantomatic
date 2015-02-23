(*  Title:      Pure/ML-Systems/share_common_data_polyml-5.3.0.ML

Dummy for Poly/ML 5.3.0, which cannot share the massive heap of HOL
anymore.
*)

structure PolyML =
struct
  open PolyML;
  fun shareCommonData _ = ();
end;
