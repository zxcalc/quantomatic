OS.FileSys.chDir "chromeui/";

PolyML.SaveState.loadState "polychrome.polyml-heap";
PolyML.exception_trace (fn() => (
use "cosy.sml"; use "cosy_ghzw.sml"
));

OS.FileSys.chDir "../";
