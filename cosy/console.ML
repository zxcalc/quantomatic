OS.FileSys.chDir "chromeui/";

PolyML.SaveState.loadState "polychrome.polyml-heap";
PolyML.exception_trace (fn() => (
use "cosy_ghzw.sml"; use "cosy.sml"
));

OS.FileSys.chDir "../";
