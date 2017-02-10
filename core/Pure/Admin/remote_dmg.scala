/*  Title:      Pure/Admin/remote_dmg.scala
    Author:     Makarius

Build dmg on remote Mac OS X system.
*/

package isabelle


object Remote_DMG
{
  def remote_dmg(ssh: SSH.Session, tar_gz_file: Path, dmg_file: Path, volume_name: String = "")
  {
    ssh.with_tmp_dir(remote_dir =>
      {
        val cd = "cd " + ssh.bash_path(remote_dir) + "; "

        ssh.write_file(remote_dir + Path.explode("dmg.tar.gz"), tar_gz_file)
        ssh.execute(cd + "mkdir root && tar -C root -xzf dmg.tar.gz").check
        ssh.execute(
          cd + "hdiutil create -srcfolder root" +
            (if (volume_name == "") "" else " -volname " + Bash.string(volume_name)) +
            " dmg.dmg").check
        ssh.read_file(remote_dir + Path.explode("dmg.dmg"), dmg_file)
      })
  }


  /* Isabelle tool wrapper */

  val isabelle_tool =
    Isabelle_Tool("remote_dmg", "build dmg on remote Mac OS X system", args =>
    {
      Command_Line.tool0 {
        var port = SSH.default_port
        var volume_name = ""

        val getopts = Getopts("""
Usage: isabelle remote_dmg [OPTIONS] USER@HOST TAR_GZ_FILE DMG_FILE

  Options are:
    -p PORT      alternative SSH port (default: """ + SSH.default_port + """)
    -V NAME      specify volume name

  Turn the contents of a tar.gz file into a dmg file -- produced on a remote
  Mac OS X system.
""",
          "p:" -> (arg => port = Value.Int.parse(arg)),
          "V:" -> (arg => volume_name = arg))

        val more_args = getopts(args)
        val (user, host, tar_gz_file, dmg_file) =
          more_args match {
            case List(SSH.Target(user, host), tar_gz_file, dmg_file) =>
              (user, host, Path.explode(tar_gz_file), Path.explode(dmg_file))
            case _ => getopts.usage()
          }

        val ssh = SSH.init_context(Options.init)
        using(ssh.open_session(user = user, host = host, port = port))(
          remote_dmg(_, tar_gz_file, dmg_file, volume_name))
      }
    }, admin = true)
}
