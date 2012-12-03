val RootDir = OS.FileSys.getDir();
OS.FileSys.chDir (RootDir ^ "/../");
use "util.ML";

val _ = RGCosy.Enum.enum RGCosy.gens 2;
