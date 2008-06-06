(*  Title:      Pure/ML-Systems/polyml-5.1.ML
    ID:         $Id: polyml-5.1.ML,v 1.10 2008/03/25 11:14:17 wenzelm Exp $

Compatibility wrapper for Poly/ML 5.1.
*)

use "ML-Systems/polyml_common.ML";
use "ML-Systems/multithreading_polyml.ML";
use "ML-Systems/polyml_old_compiler5.ML";

val pointer_eq = PolyML.pointerEq;

