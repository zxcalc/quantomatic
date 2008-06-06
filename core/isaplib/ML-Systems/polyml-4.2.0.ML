(*  Title:      Pure/ML-Systems/polyml-4.2.0.ML
    ID:         $Id: polyml-4.2.0.ML,v 1.5 2008/03/24 17:35:41 wenzelm Exp $

Compatibility wrapper for Poly/ML 4.2.0.
*)

use "ML-Systems/polyml_common.ML";
use "ML-Systems/polyml_old_compiler4.ML";

val pointer_eq = Address.wordEq;

