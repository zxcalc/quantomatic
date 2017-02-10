/*  Title:      Pure/Admin/build_release.scala
    Author:     Makarius

Build full Isabelle distribution from repository.
*/

package isabelle


object Build_Release
{
  sealed case class Bundle_Info(
    platform_family: String,
    platform_description: String,
    main_bundle: String,
    fallback_bundle: Option[String])
  {
    def all_bundles: List[String] = main_bundle :: fallback_bundle.toList
  }

  sealed case class Release_Info(
    date: Date, name: String, dist_dir: Path, dist_archive: Path, dist_library_archive: Path,
      id: String)
  {
    val bundle_infos: List[Bundle_Info] =
      List(Bundle_Info("linux", "Linux", name + "_app.tar.gz", None),
        Bundle_Info("windows", "Windows (32bit)", name + "-win32.exe", None),
        Bundle_Info("windows64", "Windows (64bit)", name + "-win64.exe", None),
        Bundle_Info("macos", "Mac OS X", name + ".dmg", Some(name + "_dmg.tar.gz")))

    def bundle_info(platform_family: String): Bundle_Info =
      bundle_infos.find(info => info.platform_family == platform_family) getOrElse
        error("Unknown platform family " + quote(platform_family))
  }


  private val default_platform_families = List("linux", "windows", "windows64", "macos")

  def build_release(base_dir: Path,
    progress: Progress = Ignore_Progress,
    rev: String = "",
    afp_rev: String = "",
    official_release: Boolean = false,
    release_name: String = "",
    platform_families: List[String] = default_platform_families,
    website: Option[Path] = None,
    build_library: Boolean = false,
    parallel_jobs: Int = 1,
    remote_mac: String = ""): Release_Info =
  {
    /* release info */

    Isabelle_System.mkdirs(base_dir)

    val release_info =
    {
      val date = Date.now()
      val name = if (release_name != "") release_name else "Isabelle_" + Date.Format.date(date)
      val dist_dir = base_dir + Path.explode("dist-" + name)
      val dist_archive = dist_dir + Path.explode(name + ".tar.gz")
      val dist_library_archive = dist_dir + Path.explode(name + "_library.tar.gz")
      Release_Info(date, name, dist_dir, dist_archive, dist_library_archive, "")
    }

    val bundle_infos = platform_families.map(release_info.bundle_info(_))


    /* make distribution */

    val jobs_option = " -j" + parallel_jobs.toString

    val release_id =
    {
      val isabelle_ident_file = base_dir + Path.explode("ISABELLE_IDENT")
      val isabelle_dist_file = base_dir + Path.explode("ISABELLE_DIST")

      if (release_info.dist_archive.is_file &&
          isabelle_ident_file.is_file && isabelle_dist_file.is_file &&
          File.eq(Path.explode(Library.trim_line(File.read(isabelle_dist_file))),
            release_info.dist_archive)) {
        progress.echo("### Release archive already exists: " + release_info.dist_archive.implode)
      }
      else {
        progress.echo("Producing release archive " + release_info.dist_archive.implode + " ...")
        progress.bash(
          "isabelle makedist -d " + File.bash_path(base_dir) + jobs_option +
            (if (official_release) " -O" else "") +
            (if (release_name != "") " -r " + Bash.string(release_name) else "") +
            (if (rev != "") " " + Bash.string(rev) else ""),
          echo = true).check
      }
      Library.trim_line(File.read(isabelle_ident_file))
    }


    /* make application bundles */

    for (info <- bundle_infos) {
      val bundle =
        (if (remote_mac.isEmpty) info.fallback_bundle else None) getOrElse info.main_bundle
      val bundle_archive = release_info.dist_dir + Path.explode(bundle)
      if (bundle_archive.is_file)
        progress.echo("### Application bundle already exists: " + bundle_archive.implode)
      else {
        progress.echo(
          "\nApplication bundle for " + info.platform_family + ": " + bundle_archive.implode)
        progress.bash(
          "isabelle makedist_bundle " + File.bash_path(release_info.dist_archive) +
            " " + Bash.string(info.platform_family) +
            (if (remote_mac == "") "" else " " + Bash.string(remote_mac)),
          echo = true).check
      }
    }


    /* minimal website */

    for (dir <- website) {
      val website_platform_bundles =
        for {
          info <- bundle_infos
          bundle <-
            info.all_bundles.find(name => (release_info.dist_dir + Path.explode(name)).is_file)
        } yield (bundle, info)

      Isabelle_System.mkdirs(dir)

      val afp_link =
        HTML.link("https://bitbucket.org/isa-afp/afp-devel/commits/" + afp_rev,
          HTML.text("AFP/" + afp_rev))

      File.write(dir + Path.explode("index.html"),
        HTML.output_document(
          List(HTML.title(release_info.name)),
          List(
            HTML.chapter(release_info.name + " (" + release_id + ")"),
            HTML.itemize(
              website_platform_bundles.map({ case (bundle, info) =>
                List(HTML.link(bundle, HTML.text(info.platform_description))) }))) :::
          (if (afp_rev == "") Nil else List(HTML.par(HTML.text("See also ") ::: List(afp_link))))))

      for ((bundle, _) <- website_platform_bundles)
        File.copy(release_info.dist_dir + Path.explode(bundle), dir)
    }


    /* HTML library */

    if (build_library) {
      if (release_info.dist_library_archive.is_file)
        progress.echo("### Library archive already exists: " +
          release_info.dist_library_archive.implode)
      else {
        Isabelle_System.with_tmp_dir("build_release")(tmp_dir =>
          {
            def execute(script: String): Unit =
              Isabelle_System.bash(script, cwd = tmp_dir.file).check

            val name = release_info.name
            val platform = Isabelle_System.getenv_strict("ISABELLE_PLATFORM_FAMILY")
            val bundle = release_info.dist_dir + Path.explode(name + "_" + platform + ".tar.gz")
            execute("tar xzf " + File.bash_path(bundle))

            val other_isabelle =
              new Other_Isabelle(progress, tmp_dir + Path.explode(name), name + "-build")

            other_isabelle.bash("bin/isabelle build" + jobs_option +
                " -o browser_info -o document=pdf -o document_variants=document:outline=/proof,/ML" +
                " -s -c -a -d '~~/src/Benchmarks'", echo = true).check
            other_isabelle.isabelle_home_user.file.delete

            execute("chmod -R a+r " + Bash.string(name))
            execute("chmod -R g=o " + Bash.string(name))
            execute("tar czf " + File.bash_path(release_info.dist_library_archive) +
              " " + Bash.string(name + "/browser_info"))
          })
      }
    }


    release_info.copy(id = release_id)
  }



  /** command line entry point **/

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      var afp_rev = ""
      var remote_mac = ""
      var official_release = false
      var release_name = ""
      var website: Option[Path] = None
      var parallel_jobs = 1
      var build_library = false
      var platform_families = default_platform_families
      var rev = ""

      val getopts = Getopts("""
Usage: Admin/build_release [OPTIONS] BASE_DIR

  Options are:
    -A REV       corresponding AFP changeset id
    -M USER@HOST remote Mac OS X for dmg build
    -O           official release (not release-candidate)
    -R RELEASE   proper release with name
    -W WEBSITE   produce minimal website in given directory
    -j INT       maximum number of parallel jobs (default 1)
    -l           build library
    -p NAMES     platform families (comma separated list, default: """ +
      default_platform_families.mkString(",") + """)
    -r REV       Mercurial changeset id (default: RELEASE or tip)

  Build Isabelle release in base directory, using the local repository clone.
""",
        "A:" -> (arg => afp_rev = arg),
        "M:" -> (arg => remote_mac = arg),
        "O" -> (_ => official_release = true),
        "R:" -> (arg => release_name = arg),
        "W:" -> (arg => website = Some(Path.explode(arg))),
        "j:" -> (arg => parallel_jobs = Value.Int.parse(arg)),
        "l" -> (_ => build_library = true),
        "p:" -> (arg => platform_families = Library.space_explode(',', arg)),
        "r:" -> (arg => rev = arg))

      val more_args = getopts(args)
      val base_dir = more_args match { case List(base_dir) => base_dir case _ => getopts.usage() }

      val progress = new Console_Progress()

      build_release(Path.explode(base_dir), progress = progress, rev = rev, afp_rev = afp_rev,
        official_release = official_release, release_name = release_name, website = website,
        platform_families =
          if (platform_families.isEmpty) default_platform_families else platform_families,
        build_library = build_library, parallel_jobs = parallel_jobs, remote_mac = remote_mac)
    }
  }
}
