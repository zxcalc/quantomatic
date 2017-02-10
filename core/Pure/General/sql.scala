/*  Title:      Pure/General/sql.scala
    Author:     Makarius

Generic support for SQL.
*/

package isabelle


import java.sql.ResultSet


object SQL
{
  /* concrete syntax */

  def quote_char(c: Char): String =
    c match {
      case '\u0000' => "\\0"
      case '\'' => "\\'"
      case '\"' => "\\\""
      case '\b' => "\\b"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\u001a' => "\\Z"
      case '\\' => "\\\\"
      case _ => c.toString
    }

  def quote_string(s: String): String =
    quote(s.map(quote_char(_)).mkString)

  def quote_ident(s: String): String =
  {
    require(!s.contains('`'))
    "`" + s + "`"
  }

  def enclosure(ss: Iterable[String]): String = ss.mkString("(", ", ", ")")


  /* columns */

  object Column
  {
    def int(name: String, strict: Boolean = true, primary_key: Boolean = false): Column[Int] =
      new Column_Int(name, strict, primary_key)
    def long(name: String, strict: Boolean = true, primary_key: Boolean = false): Column[Long] =
      new Column_Long(name, strict, primary_key)
    def double(name: String, strict: Boolean = true, primary_key: Boolean = false): Column[Double] =
      new Column_Double(name, strict, primary_key)
    def string(name: String, strict: Boolean = true, primary_key: Boolean = false): Column[String] =
      new Column_String(name, strict, primary_key)
    def bytes(name: String, strict: Boolean = true, primary_key: Boolean = false): Column[Bytes] =
      new Column_Bytes(name, strict, primary_key)
  }

  abstract class Column[+A] private[SQL](
      val name: String, val strict: Boolean, val primary_key: Boolean)
    extends Function[ResultSet, A]
  {
    def sql_name: String = quote_ident(name)
    def sql_type: String
    def sql_decl: String =
      sql_name + " " + sql_type +
      (if (strict) " NOT NULL" else "") +
      (if (primary_key) " PRIMARY KEY" else "")

    def string(rs: ResultSet): String =
    {
      val s = rs.getString(name)
      if (s == null) "" else s
    }
    def apply(rs: ResultSet): A
    def get(rs: ResultSet): Option[A] =
    {
      val x = apply(rs)
      if (rs.wasNull) None else Some(x)
    }

    override def toString: String = sql_decl
  }

  class Column_Int private[SQL](name: String, strict: Boolean, primary_key: Boolean)
    extends Column[Int](name, strict, primary_key)
  {
    def sql_type: String = "INTEGER"
    def apply(rs: ResultSet): Int = rs.getInt(name)
  }

  class Column_Long private[SQL](name: String, strict: Boolean, primary_key: Boolean)
    extends Column[Long](name, strict, primary_key)
  {
    def sql_type: String = "INTEGER"
    def apply(rs: ResultSet): Long = rs.getLong(name)
  }

  class Column_Double private[SQL](name: String, strict: Boolean, primary_key: Boolean)
    extends Column[Double](name, strict, primary_key)
  {
    def sql_type: String = "REAL"
    def apply(rs: ResultSet): Double = rs.getDouble(name)
  }

  class Column_String private[SQL](name: String, strict: Boolean, primary_key: Boolean)
    extends Column[String](name, strict, primary_key)
  {
    def sql_type: String = "TEXT"
    def apply(rs: ResultSet): String =
    {
      val s = rs.getString(name)
      if (s == null) "" else s
    }
  }

  class Column_Bytes private[SQL](name: String, strict: Boolean, primary_key: Boolean)
    extends Column[Bytes](name, strict, primary_key)
  {
    def sql_type: String = "BLOB"
    def apply(rs: ResultSet): Bytes =
    {
      val bs = rs.getBytes(name)
      if (bs == null) Bytes.empty else Bytes(bs)
    }
  }


  /* tables */

  def table(name: String, columns: List[Column[Any]]): Table = new Table(name, columns)

  class Table private[SQL](name: String, columns: List[Column[Any]])
  {
    private val columns_index: Map[String, Int] =
      columns.iterator.map(_.name).zipWithIndex.toMap

    Library.duplicates(columns.map(_.name)) match {
      case Nil =>
      case bad => error("Duplicate column names " + commas_quote(bad) + " for table " + quote(name))
    }

    columns.filter(_.primary_key) match {
      case bad if bad.length > 1 =>
        error("Multiple primary keys " + commas_quote(bad.map(_.name)) + " for table " + quote(name))
      case _ =>
    }

    def sql_create(strict: Boolean, rowid: Boolean): String =
      "CREATE TABLE " + (if (strict) "" else "IF NOT EXISTS ") +
        quote_ident(name) + " " + enclosure(columns.map(_.sql_decl)) +
        (if (rowid) "" else " WITHOUT ROWID")

    def sql_drop(strict: Boolean): String =
      "DROP TABLE " + (if (strict) "" else "IF EXISTS ") + quote_ident(name)

    def sql_create_index(
        index_name: String, index_columns: List[Column[Any]],
        strict: Boolean, unique: Boolean): String =
      "CREATE " + (if (unique) "UNIQUE " else "") + "INDEX " +
        (if (strict) "" else "IF NOT EXISTS ") + quote_ident(index_name) + " ON " +
        quote_ident(name) + " " + enclosure(index_columns.map(_.name))

    def sql_drop_index(index_name: String, strict: Boolean): String =
      "DROP INDEX " + (if (strict) "" else "IF EXISTS ") + quote_ident(index_name)

    def sql_insert: String =
      "INSERT INTO " + quote_ident(name) + " VALUES " + enclosure(columns.map(_ => "?"))

    def sql_select(select_columns: List[Column[Any]], distinct: Boolean): String =
      "SELECT " + (if (distinct) "DISTINCT " else "") +
      commas(select_columns.map(_.sql_name)) + " FROM " + quote_ident(name)

    override def toString: String =
      "TABLE " + quote_ident(name) + " " + enclosure(columns.map(_.toString))
  }


  /* results */

  def iterator[A](rs: ResultSet)(get: ResultSet => A): Iterator[A] = new Iterator[A]
  {
    private var _next: Boolean = rs.next()
    def hasNext: Boolean = _next
    def next: A = { val x = get(rs); _next = rs.next(); x }
  }
}
