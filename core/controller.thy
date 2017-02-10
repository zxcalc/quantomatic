theory controller
imports theories
begin
ML_file "json_interface/controller_util.ML"
ML_file "json_interface/controller_module.ML"
ML_file "json_interface/modules/test.ML"
ML_file "json_interface/modules/rewrite.ML"
ML_file "json_interface/modules/simplify.ML"
ML_file "json_interface/controller.ML"
ML_file "json_interface/controller_registry.ML"
ML_file "json_interface/protocol.ML"
ML_file "json_interface/run.ML"

ML_file "rewriting/simp_util.ML"
ML_file "theories/red_green/rg_simp_util.ML"

end
