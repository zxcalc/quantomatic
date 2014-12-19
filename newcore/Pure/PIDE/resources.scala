/*  Title:      Pure/PIDE/resources.scala
    Author:     Makarius

Resources for theories and auxiliary files.
*/

package isabelle


import scala.annotation.tailrec
import scala.util.parsing.input.Reader

import java.io.{File => JFile}


object Resources
{
  def thy_path(path: Path): Path = path.ext("thy")
}


class Resources(
  val loaded_theories: Set[String],
  val known_theories: Map[String, Document.Node.Name],
  val base_syntax: Prover.Syntax)
{
  /* document node names */

  def node_name(qualifier: String, raw_path: Path): Document.Node.Name =
  {
    val no_qualifier = "" // FIXME
    val path = raw_path.expand
    val node = path.implode
    val theory = Long_Name.qualify(no_qualifier, Thy_Header.thy_name(node).getOrElse(""))
    val master_dir = if (theory == "") "" else path.dir.implode
    Document.Node.Name(node, master_dir, theory)
  }


  /* file-system operations */

  def append(dir: String, source_path: Path): String =
    (Path.explode(dir) + source_path).expand.implode

  def with_thy_reader[A](name: Document.Node.Name, f: Reader[Char] => A): A =
  {
    val path = Path.explode(name.node)
    if (!path.is_file) error("No such file: " + path.toString)

    val reader = Scan.byte_reader(path.file)
    try { f(reader) } finally { reader.close }
  }


  /* theory files */

  def loaded_files(syntax: Prover.Syntax, text: String): List[String] =
    if (syntax.load_commands_in(text)) {
      val spans = Thy_Syntax.parse_spans(syntax.scan(text))
      spans.iterator.map(Thy_Syntax.span_files(syntax, _)).flatten.toList
    }
    else Nil

  private def dummy_name(theory: String): Document.Node.Name =
    Document.Node.Name(theory + ".thy", "", theory)

  def import_name(qualifier: String, master: Document.Node.Name, s: String): Document.Node.Name =
  {
    val no_qualifier = "" // FIXME
    val thy1 = Thy_Header.base_name(s)
    val thy2 = if (Long_Name.is_qualified(thy1)) thy1 else Long_Name.qualify(no_qualifier, thy1)
    (known_theories.get(thy1) orElse
     known_theories.get(thy2) orElse
     known_theories.get(Long_Name.base_name(thy1))) match {
      case Some(name) if loaded_theories(name.theory) => dummy_name(name.theory)
      case Some(name) => name
      case None =>
        val path = Path.explode(s)
        val theory = path.base.implode
        if (Long_Name.is_qualified(theory)) dummy_name(theory)
        else {
          val node = append(master.master_dir, Resources.thy_path(path))
          val master_dir = append(master.master_dir, path.dir)
          Document.Node.Name(node, master_dir, Long_Name.qualify(no_qualifier, theory))
        }
    }
  }

  def check_thy_reader(qualifier: String, name: Document.Node.Name, reader: Reader[Char])
    : Document.Node.Header =
  {
    if (reader.source.length > 0) {
      try {
        val header = Thy_Header.read(reader).decode_symbols

        val base_name = Long_Name.base_name(name.theory)
        val name1 = header.name
        if (base_name != name1)
          error("Bad file name " + Resources.thy_path(Path.basic(base_name)) +
            " for theory " + quote(name1))

        val imports = header.imports.map(import_name(qualifier, name, _))
        Document.Node.Header(imports, header.keywords, Nil)
      }
      catch { case exn: Throwable => Document.Node.bad_header(Exn.message(exn)) }
    }
    else Document.Node.no_header
  }

  def check_thy(qualifier: String, name: Document.Node.Name): Document.Node.Header =
    with_thy_reader(name, check_thy_reader(qualifier, name, _))


  /* document changes */

  def parse_change(
      reparse_limit: Int,
      previous: Document.Version,
      doc_blobs: Document.Blobs,
      edits: List[Document.Edit_Text]): Session.Change =
    Thy_Syntax.parse_change(this, reparse_limit, previous, doc_blobs, edits)

  def commit(change: Session.Change) { }


  /* prover process */

  def start_prover(receiver: Prover.Message => Unit, name: String, args: List[String]): Prover =
    new Isabelle_Process(receiver, args) with Protocol
}

