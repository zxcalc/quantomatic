(*   *)
val rule = TheoryHack.frobenius_rule
val tgt = g;

Rule.print rule;
RGGraph.print g;

open DB_Rule;
                                                    
      val (rn,lhs2) = RGGraph.rename_apart g (get_lhs rule);

RGGraph.print (get_lhs rule);
RGGraph.print lhs2;

val (vrn,ern) = rn;
Vertex.print_renaming vrn;
Edge.print_renaming ern;

(* RAISES ERROR: 
      val (rn2, rhs2) = RGGraph.rename rn (get_rhs rule) *)

val g = (get_rhs rule);

RGGraph.print g;

      val vnames = (RGGraph.get_vnames g);
Vertex.NSet.print vnames;
      val fst_vnames = Vertex.NSet.intersect 
                         vnames (Vertex.newnames_of_renaming vrn);
Vertex.NSet.print fst_vnames;
      val snd_vnames = Vertex.NSet.subtract vnames fst_vnames;
Vertex.NSet.print snd_vnames;

      val enames = (RGGraph.get_enames g);
Edge.NSet.print enames;
      val fst_enames = Edge.NSet.intersect enames (Edge.newnames_of_renaming ern);
Edge.NSet.print fst_enames;
      val snd_enames = Edge.NSet.subtract enames fst_enames;
Edge.NSet.print snd_enames;


      val (vrn2,g2) = RGGraph.rename_vnames fst_vnames (vrn, g);
Vertex.print_renaming vrn2;
RGGraph.print g2;
      val (vrn3,g3) = RGGraph.rename_vnames snd_vnames (vrn2, g2);
Vertex.print_renaming vrn3;
RGGraph.print g3;

Edge.print_renaming ern;
Edge.NSet.print fst_enames;
      val (ern2,g4) = RGGraph.rename_enames fst_enames (ern, g3);
      val (ern3,g5) = RGGraph.rename_enames snd_enames (ern2, g4);

val e = "Ef" : Edge.name;
val (ern2,g') = (ern,g3);
Edge.rename1 e ern2

val _ = Pretty.writeln (Pretty.str "Tests passed: rewriting with rules.");

(* end; (* structure *) *)


