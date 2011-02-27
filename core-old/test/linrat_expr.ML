(*
install_pp (make_pp ["LinratExpr", "T"] (Pretty.pprint o LinratExpr.pretty));
*)

structure linrat_expr_test_vals = 
struct
structure exp = DB_LinratExpr;
structure var = VName;
structure n = exp.Coeff;

val zero = exp.zero;
val pi = exp.pi;

val a_v = "a";
val b_v = "b";
val c_v = "c";

val a = exp.mk_var a_v;
val b = exp.mk_var b_v;
val c = exp.mk_var c_v;

val u = n.mk ~1 1
val w = n.mk 1 6;
val x = n.mk ~2 3;
val y = n.mk 7 8;
val z = n.mk 0 1;

fun op +(x,y) = exp.add_expr x y;
fun op -(x,y) = exp.subtr_expr x y;
fun op *(k,x) = exp.scale_expr k x;

val tab1a = var.NTab.update (a_v, pi) var.NTab.empty;
val tab1b = var.NTab.update (b_v, x*pi) var.NTab.empty;
val tab1c = var.NTab.update (c_v, zero) var.NTab.empty;
val tab1ab = var.NTab.update (b_v, pi) tab1a;
val tab1bc = var.NTab.update (c_v, x*pi) tab1b;
val tab1ca = var.NTab.update (a_v, zero) tab1c;

val tab2a = var.NTab.update (a_v, pi+x*c) var.NTab.empty;
val tab2b = var.NTab.update (b_v, x*pi+u*a) var.NTab.empty;
val tab2c = var.NTab.update (c_v, a) var.NTab.empty;
val tab2ab = var.NTab.update (b_v, pi+c) tab2a;
val tab2bc = var.NTab.update (c_v, x*pi) tab2b;
val tab2ca = var.NTab.update (a_v, zero) tab2c;

end;

          (* testing match:
           val SOME x = exp.match a zero var.NTab.empty;
               (VName.NTab.print exp.pretty) x;
           *)
