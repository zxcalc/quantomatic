structure TheoryDir =
struct

fun find_rewrites dir = let
    val dstream = OS.FileSys.openDir (dir ^ "/rewrites")
    fun ls () = case OS.FileSys.readDir dstream
              of SOME f => if OS.FileSys.isDir (dir ^ "/rewrites/" ^ f)
                           then f :: ls ()
                           else ls ()
               | NONE => []
in ls ()
end

fun load_rule dir name = let
    val lhs = RGGraphLoaderSaver.load_graph (dir ^ "/rewrites/" ^ name ^ "/lhs.graph")
    val rhs = RGGraphLoaderSaver.load_graph (dir ^ "/rewrites/" ^ name ^ "/rhs.graph")
in (name, Rule.mk (lhs,rhs))
end

fun load_rules dir = map (load_rule dir) (find_rewrites dir)

end
