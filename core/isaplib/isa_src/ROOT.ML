(*  Title:      Pure/ROOT.ML
    ID:         $Id: ROOT.ML,v 1.158 2008/04/12 15:00:42 wenzelm Exp $

Pure Isabelle.
*)

val version = "Isabelle Library - 2008";

print_depth 10;

cd "isa_src";
use "basics.ML";
use "library.ML";
use "print_mode.ML";
use "alist.ML";
use "table.ML";
use "output.ML";
use "markup.ML";
use "scan.ML";
use "source.ML";
use "symbol.ML";
use "seq.ML";
use "position.ML";
use "ml_lex.ML";
use "ml_parse.ML";
use "secure.ML";
use "integer.ML";
use "stack.ML";
use "heap.ML";
use "ord_list.ML";
use "balanced_tree.ML";
use "graph.ML";
use "name_space.ML";
use "susp.ML";
use "path.ML";
use "url.ML";
use "file.ML";
use "buffer.ML";
use "history.ML";
use "xml.ML";
use "yxml.ML";
use "pretty.ML";
cd "..";
