(* 
USAGE:

use "core/dev/convert_old_files.ML";

update_graphs_in_directory "examples";

*)
structure File = 
struct
  open File;

  (* get a list of all files in a dir *)
  fun ls d = 
    let 
      val dirstream = OS.FileSys.openDir d;
      fun ls_f l = 
        (case OS.FileSys.readDir dirstream of NONE => l
         | SOME p => ls_f (p::l));
      val files = ls_f [];
      val _ = OS.FileSys.closeDir dirstream;
    in files end;
end;

fun file_exists filename = 
    ((OS.FileSys.fileId filename; true) handle OS.SysErr _ => false); 

(* given a filename, name sure we can rename it FILENAME.old; returns 
   {old,new} record of old name and new suggsted renamed name.
   If there's already FILENAME.old raise an error. *)
fun get_file_renaming filename = 
    let 
      val {base=base_name,ext=ext_name} = OS.Path.splitBaseExt filename;
      val backup_name = OS.Path.joinBaseExt {base = filename, ext = SOME "old"};
    in 
      if file_exists backup_name then 
         raise error "Backup file alredy exists. Cancelling update_file"
      else 
        {old=filename, new=backup_name}
    end;

(* Assumes that "old" (file name) does not exist, but can be written to, 
   and "new" filename can be read from. Renames "old" to "new" and gives back record: {input=old,output=new}  
*)
fun rename_prep (x as {new, old}) = 
    let val _ = OS.FileSys.rename {old=old,new=new}; 
    in {input = new, output=old} end;
 
(* updating file formats *)
fun update_ruleset {input,output} =  
    input |> XMLReader.read_from_file
          |> Test_IO_Interface_v1.Input.Ruleset.input
          |> Test_IO_Interface_v2.Output.Ruleset.output
          |> XMLWriter.write_to_file output;

fun update_graph {input,output} =  
    input |> XMLReader.read_from_file
          |> Test_IO_Interface_v1.Input.Graph.input
          |> Test_IO_Interface_v2.Output.Graph.output
          |> XMLWriter.write_to_file output;

(* *)
fun update_graphs_in_directory d = 
    let 
      val files = File.ls d;
      val graph_files = 
          filter (fn s => (#ext (OS.Path.splitBaseExt s)) = SOME "graph") files;
      val graph_file_renamings = 
          map (get_file_renaming 
               o (fn n => OS.Path.joinDirFile {dir=d,file=n})) 
              graph_files;
    in map (update_graph o rename_prep) graph_file_renamings end;

fun update_rulesets_in_directory d = 
    let 
      val files = File.ls d;
      val graph_files = 
          filter (fn s => (#ext (OS.Path.splitBaseExt s)) = SOME "rules") files;
      val graph_file_renamings = 
          map (get_file_renaming 
               o (fn n => OS.Path.joinDirFile {dir=d,file=n})) 
              graph_files;
    in map (update_ruleset o rename_prep) graph_file_renamings end;


